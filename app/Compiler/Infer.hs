{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Infer (checkModule) where

import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bf
import qualified Data.Foldable as Fold
import qualified Text.Read as Read
import qualified Control.Monad as Monad
import Control.Monad ((>=>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set, (\\))

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)

import qualified Control.Lens as Lens

import qualified Compiler.Error as Error
import Syntax.Desugared
import Syntax.Common

import Debug.Trace

pattern TCall2 t1 t2 t3 = TCall (TCall t1 t2) t3
pattern TCall3 t1 t2 t3 t4 = TCall (TCall2 t1 t2 t3) t4

data Env = Env
  { _typeEnv :: Map Identifier Scheme
  , _kindEnv :: Map Identifier Kind
  , _synonyms :: Map Identifier ([Name], Type)
  , _syntax :: Map Role Identifier
  }

Lens.makeLenses ''Env

instance Semigroup Env where
  Env t1 k1 s1 x1 <> Env t2 k2 s2 x2 =
    Env (t1 <> t2) (k1 <> k2) (s1 <> s2) (x1 <> x2)

instance Monoid Env where
  mempty = Env Map.empty Map.empty Map.empty Map.empty

type Subst = Map Name Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s2 s1 = s2 <> apply s2 s1

class Substitutable a where
  apply :: Subst -> a -> a
  freeVars :: a -> Set Name

instance Substitutable Type where
  apply s = \case
    TCon n -> TCon n
    TLabel n -> TLabel n
    t@(TVar (n, _)) -> Maybe.fromMaybe t (Map.lookup n s)
    TCall t1 t2 -> TCall (apply s t1) (apply s t2)

  freeVars = \case
    TCon _ -> Set.empty
    TLabel _ -> Set.empty
    TVar (n, _) -> Set.singleton n
    TCall t1 t2 -> freeVars t1 <> freeVars t2

instance Substitutable Scheme where
  apply s (Forall vs t) = Forall vs (apply s' t)
    where s' = foldr Map.delete s vs

  freeVars (Forall vs t) = freeVars t \\ Set.fromList vs

instance Substitutable a => Substitutable (Map k a) where
  apply s = fmap (apply s)
  freeVars = foldMap freeVars

type Constraint = (Type, Type)

data Stream a = Cons a (Stream a)

data InferState = InferState
  { _constraints :: [Constraint]
  , _freshNames :: Stream Name
  }

Lens.makeLenses ''InferState

type Infer e = ExceptT e (ReaderT Env (State InferState))

extendEnv :: Identifier -> Scheme -> Infer e a -> Infer e a
extendEnv n t = Reader.local $ Lens.over typeEnv (Map.insert n t)

extendKindEnv :: Identifier -> Kind -> Infer e a  -> Infer e a
extendKindEnv n k = Reader.local $ Lens.over kindEnv (Map.insert n k)

specialType :: Role -> Infer String Type -> Infer String Type
specialType role err = do
  special <- Reader.asks (Lens.view syntax)
  case Map.lookup role special of
    Nothing -> err
    Just n -> unaliasType (TCon n)

functionType :: Type -> Type -> Infer String Type
functionType t1 t2 = do
  t <- specialType FunctionType Error.functionType
  return (TCall2 t t1 t2)

numberType :: Type -> Infer String Type
numberType t1 = do
  t <- specialType NumberType Error.numberType
  return (TCall t t1)

floatType :: Infer String Type
floatType = specialType FloatType Error.floatType

charType :: Infer String Type
charType = specialType CharType Error.charType

stringType :: Infer String Type
stringType = specialType StringType Error.stringType

labelType :: Label -> Infer String Type
labelType n = do
  t <- specialType LabelType Error.labelType
  return $ TCall t (TLabel n)

rowCons :: Infer String (Maybe Identifier)
rowCons = Reader.asks (Map.lookup RowConstructor . Lens.view syntax)

instantiate :: Origin -> Scheme -> Infer e Type
instantiate origin (Forall vs t) = do
  vs' <- mapM (const $ freshVar origin) vs
  let s = Map.fromList $ zip vs vs'
  return (apply s t)

generalise :: Type -> Infer e Scheme
generalise t = do
  env <- Reader.asks (Lens.view typeEnv)
  let vs = freeVars t \\ freeVars env
  return $ Forall (Set.toList vs) t

unify :: Type -> Type -> Infer e ()
unify t1 t2 = State.modify $ Lens.over constraints ((t1, t2) :)

freshVar :: Origin -> Infer e Type
freshVar origin = do
  var <- State.gets (first . Lens.view freshNames)
  State.modify $ Lens.over freshNames rest
  return $ TVar (var, origin)
  where
    first (Cons n _) = n
    rest (Cons _ ns) = ns

inferType :: Expr -> Infer String Type
inferType = \case
  Int _ -> freshVar Inferred >>= numberType
  Float _ -> floatType
  Char _ -> charType
  String _ -> stringType
  Label n -> labelType n

  Identifier n -> do
    env <- Reader.asks (Lens.view typeEnv)
    maybe (Error.notDefined n) (instantiate Inferred) (Map.lookup n env)

  DefIn n anno x1 x2 -> do
    var <- freshVar Inferred
    t <- extendEnv (Unqualified n) (Forall [] var) (inferType x1)
    unify t var
    anno' <- mapM (unaliasScheme >=> instantiate Annotated) anno
    mapM_ (unify t) anno'
    s <- solve
    equivalent anno' $ fmap (apply s) anno'
    Reader.local (Lens.over typeEnv $ apply s) do
      sc <- generalise (apply s t)
      extendEnv (Unqualified n) sc (inferType x2)

  Lambda n x -> do
    var <- freshVar Inferred
    t <- extendEnv (Unqualified n) (Forall [] var) (inferType x)
    functionType var t

  Call x1 x2 -> do
    t1 <- inferType x1
    t2 <- inferType x2
    var <- freshVar Inferred
    functionType t2 var >>= unify t1
    return var

unifies :: Maybe Identifier -> Type -> Type -> Infer String Subst
unifies cons = curry \case
  (t1, t2) | t1 == t2 -> return nullSubst

  (TVar (n, _), t) -> bind n t
  (t, TVar (n, Inferred)) -> bind n t
  (t, TVar (n, Annotated)) -> Error.generalAnno

  (TCall3 (TCon n) l v1 r1, t2) | Just cons' <- cons, n == cons' -> do
      (v2, r2) <- rowGet cons' l t2
      s1 <- unifies cons v1 v2
      s2 <- unifies cons (apply s1 r1) (apply s1 r2)
      return (compose s2 s1)

  (TCall t1 t2, TCall t3 t4) -> do
    s1 <- unifies cons t1 t3
    s2 <- unifies cons (apply s1 t2) (apply s1 t4)
    return (compose s2 s1)

  (t1, t2) -> Error.unification t1 t2

rowGet :: Identifier -> Type -> Type -> Infer String (Type, Type)
rowGet cons label = \case
  (TCall3 (TCon n) l v r) | n == cons ->
    if l == label then return (v, r)
    else Bf.second (TCall3 (TCon n) l v) <$> rowGet cons label r

  TVar (_, origin) -> (,) <$> freshVar origin <*> freshVar origin

  t -> Error.noLabel label t

bind :: Name -> Type -> Infer String Subst
bind n t
  | Set.member n (freeVars t) = Error.occursCheck
  | otherwise = return (Map.singleton n t)

solver :: Subst -> [Constraint] -> Infer String Subst
solver s = \case
  [] -> return s
  (t1, t2) : cs -> do
    cons <- rowCons
    s1 <- unifies cons t1 t2
    let sub = Bf.bimap (apply s1) (apply s1)
    solver (compose s1 s) (map sub cs)

solve :: Infer String Subst
solve = State.gets (Lens.view constraints) >>= solver nullSubst

kindMapUnion :: Map Name Kind -> Map Name Kind -> Infer String (Map Name Kind)
kindMapUnion s1 s2 = do
  sequence_ (Map.mapWithKey checkEqual s1)
  return (s1 <> s2)
  where
    checkEqual n k =
      case Map.lookup n s2 of
        Nothing -> return () :: Infer String ()
        Just k1 | k == k1 -> return ()
        _ -> Error.kindCheck n

inferKind :: Type -> Infer String (Kind, Map Name Kind)
inferKind = \case
  TCon n -> do
    env <- Reader.asks (Lens.view kindEnv)
    case Map.lookup n env of
      Nothing -> Error.notDefined n
      Just k -> return (k, Map.empty)

  TLabel n -> return (KLabel, Map.empty)
  TVar n -> Error.typeVariables

  TCall t1 t2 -> do
    (k1, s1) <- inferKind t1
    case k1 of
      KArr k2 k3 -> do
        s2 <- checkKind k2 t2
        s3 <- kindMapUnion s1 s2
        return (k3, s3)
      _ -> Error.typeCall t1

checkKind :: Kind -> Type -> Infer String (Map Name Kind)
checkKind k = \case
  TVar (n, _) -> return (Map.singleton n k)

  t -> do
    (k1, s) <- inferKind t
    if k == k1
      then return s
      else Error.kindUnification k k1

checkFuncs :: [(Identifier, Maybe Scheme, Expr)] -> Infer Error.Context ()
checkFuncs = \case
  [] -> return ()
  (n, anno, x) : fs -> do
    (s, t) <- Error.defContext n do
      var <- freshVar Inferred
      t <- extendEnv n (Forall [] var) (inferType x)
      unify t var
      anno' <- mapM (unaliasScheme >=> instantiate Annotated) anno
      mapM_ (unify t) anno'
      s <- solve
      equivalent anno' $ fmap (apply s) anno'
      return (s, t)
    Reader.local (Lens.over typeEnv $ apply s) $ do
      sc <- generalise (apply s t)
      State.modify $ Lens.over constraints (const [])
      extendEnv n sc (checkFuncs fs)

equivalent :: Maybe Type -> Maybe Type -> Infer String ()
equivalent = curry \case
  (Nothing, _) -> return ()
  (_, Nothing) -> return ()
  (Just t1, Just t2) ->
    let
      pairings = curry \case
        (TCall t1 t2, TCall t3 t4) -> pairings t1 t3 <> pairings t2 t4
        (TVar (v1, _), TVar (v2, _)) -> Set.singleton (v1, v2)
        _ -> Set.empty

      pairs = pairings t1 t2

      bad =
        Set.size (Set.map fst pairs) < Set.size pairs ||
        Set.size (Set.map snd pairs) < Set.size pairs
    in
    Monad.when bad (Error.unification t1 t2)

unaliasType :: Type -> Infer String Type
unaliasType t = do
  syns <- Reader.asks (Lens.view synonyms)
  case getFunc t of
    Nothing -> keepLooking
    Just n ->
      case Map.lookup n syns of
        Nothing -> keepLooking
        Just (as, t2) ->
          case compare (length as) (length args) of
            LT -> keepLooking
            GT -> Error.partialApplication n
            EQ -> do
              args' <- mapM unaliasType args
              return $ apply (Map.fromList $ zip as args') t2

  where
    args = getArgs t
    keepLooking =
      case t of
        TCon _ -> return t
        TLabel _ -> return t
        TVar _ -> return t
        TCall t1 t2 -> TCall
          <$> unaliasType t1
          <*> unaliasType t2

unaliasScheme :: Scheme -> Infer String Scheme
unaliasScheme (Forall vs t) = Forall vs <$> unaliasType t

getFunc :: Type -> Maybe Identifier
getFunc = \case
  TCon n -> Just n
  TLabel _ -> Nothing
  TVar _ -> Nothing
  TCall t1 _ -> getFunc t1

getArgs :: Type -> [Type]
getArgs = \case
  TCon _ -> []
  TLabel _ -> []
  TVar _ -> []
  TCall t1 t2 -> getArgs t1 ++ [t2]

synonymMap :: [(Identifier, [Name], Type)] -> Infer Error.Context (Map Identifier ([Name], Type))
synonymMap = Fold.foldrM addSynonym Map.empty

addSynonym :: (Identifier, [Name], Type) -> Map Identifier ([Name], Type) -> Infer Error.Context (Map Identifier ([Name], Type))
addSynonym (n, as, t) syns = do
  Error.defContext n do
    syns' <- withSyn $ mapM (secondM unaliasType) syns
    return (syn <> syns')
  where
    syn = Map.singleton n (as, t)
    withSyn = Reader.local $ Lens.over synonyms (const syn)

    secondM f (x, y) = do
      y' <- f y
      return (x, y')

wrapperEnv :: Identifier -> (Kind, Maybe Wrapper) -> Infer Error.Context Env
wrapperEnv n (_, w) =
  case w of
    Nothing -> return mempty
    Just (Wrapper vs t maker getter) ->
      let
        typeVar v = TVar (v, Annotated)
        wrapped = foldl TCall (TCon n) (map typeVar vs)
      in
      Error.defContext n do
        t' <- unaliasType t
        checkKind KType wrapped
        checkKind KType t'
        makerType <- Forall vs <$> functionType t' wrapped
        getterType <- Forall vs <$> functionType wrapped t'
        let makerEnv = Map.singleton maker makerType
        let typeEnv = case getter of
              Nothing -> makerEnv
              Just gt -> Map.insert gt getterType makerEnv
        return (Env typeEnv Map.empty Map.empty Map.empty)

checkModule :: Module -> Either Error.Context ()
checkModule (Module funcs foreigns types syns syntax) =
  State.evalState (Reader.runReaderT (Except.runExceptT check) env) state
  where
    state = InferState [] (foldr Cons undefined fresh)
    env = Env Map.empty (fmap fst types) Map.empty syntax

    prefixes = map (:[]) ['a'..'z']
    numbers = concatMap (replicate $ length prefixes) [0..]
    fresh = zipWith (++) (cycle prefixes) (map show numbers)

    unaliasForeign n (t, _) =
      Error.defContext n (unaliasScheme t)

    checkForeign n (Forall _ t) =
      Error.defContext n $ Monad.void (checkKind KType t)

    check = do
      syns' <- synonymMap syns
      Reader.local (Lens.over synonyms $ const syns') do
        foreigns' <- sequence (Map.mapWithKey unaliasForeign foreigns)
        let foreignE = Env foreigns' Map.empty Map.empty Map.empty
        wrapperE <- Fold.fold <$> sequence (Map.mapWithKey wrapperEnv types)
        Reader.local ((foreignE <> wrapperE) <>) do
          sequence_ (Map.mapWithKey checkForeign foreigns')
          checkFuncs funcs
