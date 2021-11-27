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
import Data.Function ((&))

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
import qualified Syntax.Desugared as D
import Syntax.Common
import Syntax.Desugared (SimpleExpr(..), Expr, Wrapper(..), Module(..))
import Syntax.Inferred

import Debug.Trace

convertType :: D.Type -> Type
convertType (tipe, _) =
  case tipe of
    D.TCon n -> TCon n
    D.TVar n -> TVar (n, Annotated)
    D.TLabel n -> TLabel n
    D.TCall t1 t2 -> TCall (convertType t1) (convertType t2)

convertScheme :: D.Scheme -> Scheme
convertScheme (D.Forall vs t) = Forall vs (convertType t)

pattern TCall2 t1 t2 t3 = TCall (TCall t1 t2) t3
pattern TCall3 t1 t2 t3 t4 = TCall (TCall2 t1 t2 t3) t4

data Env = Env
  { _typeEnv :: Map Identifier Scheme
  , _kindEnv :: Map Identifier Kind
  , _synonyms :: Map Identifier ([Name], D.Type)
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

type Constraint = (Location, Type, Type)

data Stream a = Cons a (Stream a)

data InferState = InferState
  { _constraints :: [Constraint]
  , _freshNames :: Stream Name
  }

Lens.makeLenses ''InferState

type InferWith e = ExceptT e (ReaderT Env (State InferState))
type Infer = InferWith Error.Msg

extendEnv :: Identifier -> Scheme -> Infer a -> Infer a
extendEnv n t = Reader.local $ Lens.over typeEnv (Map.insert n t)

extendKindEnv :: Identifier -> Kind -> Infer a  -> Infer a
extendKindEnv n k = Reader.local $ Lens.over kindEnv (Map.insert n k)

specialType :: Role -> Location -> InferWith String Type -> Infer Type
specialType role loc err = do
  special <- Reader.asks (Lens.view syntax)
  case Map.lookup role special of
    Nothing -> Error.withLocation loc err
    Just n -> convertType <$> unaliasType (D.TCon n, loc)

functionType :: Location -> Type -> Type -> Infer Type
functionType loc t1 t2 = do
  t <- specialType FunctionType loc Error.functionType
  return (TCall2 t t1 t2)

numberType :: Location -> Type -> Infer Type
numberType loc t1 = do
  t <- specialType NumberType loc Error.numberType
  return (TCall t t1)

floatType :: Location -> Infer Type
floatType loc = specialType FloatType loc Error.floatType

charType :: Location -> Infer Type
charType loc = specialType CharType loc Error.charType

stringType :: Location -> Infer Type
stringType loc = specialType StringType loc Error.stringType

labelType :: Location -> Label -> Infer Type
labelType loc n = do
  t <- specialType LabelType loc Error.labelType
  return $ TCall t (TLabel n)

rowCons :: Infer (Maybe Identifier)
rowCons = Reader.asks (Map.lookup RowConstructor . Lens.view syntax)

instantiate :: Origin -> Scheme -> Infer Type
instantiate origin (Forall vs t) = do
  vs' <- mapM (const $ freshVar origin) vs
  let s = Map.fromList $ zip vs vs'
  return (apply s t)

generalise :: Type -> Infer Scheme
generalise t = do
  env <- Reader.asks (Lens.view typeEnv)
  let vs = freeVars t \\ freeVars env
  return $ Forall (Set.toList vs) t

unify :: Location -> Type -> Type -> Infer ()
unify loc t1 t2 = State.modify $ Lens.over constraints ((loc, t1, t2) :)

freshVar :: Origin -> Infer Type
freshVar origin = do
  var <- State.gets (first . Lens.view freshNames)
  State.modify $ Lens.over freshNames rest
  return $ TVar (var, origin)
  where
    first (Cons n _) = n
    rest (Cons _ ns) = ns

inferType :: Expr -> Infer Type
inferType (expr, loc, _) =
  case expr of
    Int _ -> freshVar Inferred >>= numberType loc
    Float _ -> floatType loc
    Char _ -> charType loc
    String _ -> stringType loc
    Label n -> labelType loc n

    Identifier n -> do
      env <- Reader.asks (Lens.view typeEnv)
      maybe (Error.withLocation loc $ Error.notDefined n)
        (instantiate Inferred) (Map.lookup n env)

    DefIn n maybeAnno x1@(_, loc1, _) x2 -> do
      (s, t) <- do
        var <- freshVar Inferred
        t <- extendEnv (Unqualified n) (Forall [] var) (inferType x1)
        unify loc1 t var
        case maybeAnno of
          Nothing -> do
            s <- solve
            return (s, t)
          Just anno@(D.Forall _ (_, annoLoc)) -> do
            anno' <- instantiate Annotated . convertScheme =<< unaliasScheme anno
            unify loc1 t anno'
            s <- solve
            equivalent annoLoc anno' (apply s anno')
            return (s, t)
      Reader.local (Lens.over typeEnv $ apply s) do
        sc <- generalise (apply s t)
        extendEnv (Unqualified n) sc (inferType x2)

    Lambda n x -> do
      var <- freshVar Inferred
      t <- extendEnv (Unqualified n) (Forall [] var) (inferType x)
      functionType loc var t

    Call x1@(_, loc1, _) x2@(_, loc2, _) -> do
      t1 <- inferType x1
      t2 <- inferType x2
      var <- freshVar Inferred
      functionType loc1 t2 var >>= unify loc2 t1
      return var

unifies :: Maybe Identifier -> Location -> Type -> Type -> Infer Subst
unifies cons loc = curry \case
  (t1, t2) | t1 == t2 -> return nullSubst

  (TVar (n, _), t) -> bind loc n t
  (t, TVar (n, Inferred)) -> bind loc n t
  (t, TVar (n, Annotated)) -> Error.withLocation loc Error.generalAnno

  (TCall3 (TCon n) l v1 r1, t2) | Just cons' <- cons, n == cons' -> do
      (v2, r2) <- rowGet cons' loc l t2
      s1 <- unifies cons loc v1 v2
      s2 <- unifies cons loc (apply s1 r1) (apply s1 r2)
      return (compose s2 s1)

  (TCall t1 t2, TCall t3 t4) -> do
    s1 <- unifies cons loc t1 t3
    s2 <- unifies cons loc (apply s1 t2) (apply s1 t4)
    return (compose s2 s1)

  (t1, t2) -> Error.withLocation loc (Error.unification t1 t2)

rowGet :: Identifier -> Location -> Type -> Type -> Infer (Type, Type)
rowGet cons loc label = \case
  (TCall3 (TCon n) l v r) | n == cons ->
    if l == label then return (v, r)
    else Bf.second (TCall3 (TCon n) l v) <$> rowGet cons loc label r

  TVar (_, origin) -> (,) <$> freshVar origin <*> freshVar origin

  t -> Error.withLocation loc (Error.noLabel label t)

bind :: Location -> Name -> Type -> Infer Subst
bind loc n t
  | Set.member n (freeVars t) = Error.withLocation loc Error.occursCheck
  | otherwise = return (Map.singleton n t)

solver :: Subst -> [Constraint] -> Infer Subst
solver s = \case
  [] -> return s
  (loc, t1, t2) : cs -> do
    cons <- rowCons
    s1 <- unifies cons loc t1 t2
    let sub = Bf.bimap (apply s1) (apply s1)
    solver (compose s1 s) (map sub cs)

solve :: Infer Subst
solve = State.gets (Lens.view constraints) >>= solver nullSubst

kindMapUnion :: Location -> Map Name Kind -> Map Name Kind -> Infer (Map Name Kind)
kindMapUnion loc s1 s2 = do
  sequence_ (Map.mapWithKey checkEqual s1)
  return (s1 <> s2)
  where
    checkEqual n k =
      case Map.lookup n s2 of
        Nothing -> return () :: Infer ()
        Just k1 | k == k1 -> return ()
        _ -> Error.withLocation loc (Error.kindCheck n)

inferKind :: D.Type -> Infer (Kind, Map Name Kind)
inferKind (tipe, loc) =
  case tipe of
    D.TCon n -> do
      env <- Reader.asks (Lens.view kindEnv)
      case Map.lookup n env of
        Nothing -> Error.withLocation loc (Error.notDefined n)
        Just k -> return (k, Map.empty)

    D.TLabel n -> return (KLabel, Map.empty)
    D.TVar n -> Error.withLocation loc Error.typeVariables

    D.TCall t1@(_, loc1) t2 -> do
      (k1, s1) <- inferKind t1
      case k1 of
        KArr k2 k3 -> do
          s2 <- checkKind k2 t2
          s3 <- kindMapUnion loc s1 s2
          return (k3, s3)
        _ -> Error.withLocation loc1 $ Error.typeCall (convertType t1)

checkKind :: Kind -> D.Type -> Infer (Map Name Kind)
checkKind k = \case
  (D.TVar n, _) -> return (Map.singleton n k)

  t@(_, loc) -> do
    (k1, s) <- inferKind t
    if k == k1
      then return s
      else Error.withLocation loc (Error.kindUnification k k1)

checkFuncs :: [(Identifier, Maybe D.Scheme, Expr, Location)] -> Infer ()
checkFuncs = \case
  [] -> return ()
  (n, maybeAnno, x@(_, loc, _), _) : fs -> do
    (s, t) <- Error.defContext n do
      var <- freshVar Inferred
      t <- extendEnv n (Forall [] var) (inferType x)
      unify loc t var
      case maybeAnno of
        Nothing -> do
          s <- solve
          return (s, t)
        Just anno@(D.Forall _ (_, annoLoc)) -> do
          anno' <- instantiate Annotated . convertScheme =<< unaliasScheme anno
          unify loc t anno'
          s <- solve
          equivalent annoLoc anno' (apply s anno')
          return (s, t)
    Reader.local (Lens.over typeEnv $ apply s) $ do
      sc <- generalise (apply s t)
      State.modify $ Lens.over constraints (const [])
      extendEnv n sc (checkFuncs fs)

equivalent :: Location -> Type -> Type -> Infer ()
equivalent loc t1 t2 =
  Monad.when bad $ Error.withLocation loc (Error.unification t1 t2)
  where
    pairings = curry \case
      (TCall t1 t2, TCall t3 t4) -> pairings t1 t3 <> pairings t2 t4
      (TVar (v1, _), TVar (v2, _)) -> Set.singleton (v1, v2)
      _ -> Set.empty

    pairs = pairings t1 t2

    bad =
      Set.size (Set.map fst pairs) < Set.size pairs ||
      Set.size (Set.map snd pairs) < Set.size pairs

applyDesugared :: Map Name D.Type -> D.Type -> D.Type
applyDesugared s (tipe, loc) =
  case tipe of
    D.TCon n -> (D.TCon n, loc)
    D.TLabel n -> (D.TLabel n, loc)
    D.TVar n -> Maybe.fromMaybe (D.TVar n, loc) (Map.lookup n s)
    D.TCall t1 t2 ->
      let
        t1' = applyDesugared s t1
        t2' = applyDesugared s t2
      in
      (D.TCall t1' t2', loc)

unaliasType :: D.Type -> Infer D.Type
unaliasType t = do
  syns <- Reader.asks (Lens.view synonyms)
  case getFunc t of
    Nothing -> keepLooking
    Just (n, loc) ->
      case Map.lookup n syns of
        Nothing -> keepLooking
        Just (as, t2) ->
          case compare (length as) (length args) of
            LT -> keepLooking
            GT -> Error.withLocation loc (Error.partialApplication n)
            EQ -> do
              args' <- mapM unaliasType args
              return $ applyDesugared (Map.fromList $ zip as args') t2

  where
    args = getArgs t
    keepLooking =
      case fst t of
        D.TCon _ -> return t
        D.TLabel _ -> return t
        D.TVar _ -> return t
        D.TCall t1 t2 -> do
          t1' <- unaliasType t1
          t2' <- unaliasType t2
          return (D.TCall t1' t2', snd t)

unaliasScheme :: D.Scheme -> Infer D.Scheme
unaliasScheme (D.Forall vs t) = D.Forall vs <$> unaliasType t

getFunc :: D.Type -> Maybe (Identifier, Location)
getFunc (tipe, loc) =
  case tipe of
    D.TCon n -> Just (n, loc)
    D.TLabel _ -> Nothing
    D.TVar _ -> Nothing
    D.TCall t1 _ -> getFunc t1

getArgs :: D.Type -> [D.Type]
getArgs (tipe, _) =
  case tipe of
    D.TCon _ -> []
    D.TLabel _ -> []
    D.TVar _ -> []
    D.TCall t1 t2 -> getArgs t1 ++ [t2]

synonymMap :: [(Identifier, [Name], D.Type, Location)] -> Infer (Map Identifier ([Name], D.Type))
synonymMap = Fold.foldrM addSynonym Map.empty

addSynonym :: (Identifier, [Name], D.Type, Location) -> Map Identifier ([Name], D.Type) -> Infer (Map Identifier ([Name], D.Type))
addSynonym (n, as, t, _) syns = do
  Error.defContext n do
    syns' <- withSyn $ mapM (secondM unaliasType) syns
    return (syn <> syns')
  where
    syn = Map.singleton n (as, t)
    withSyn = Reader.local $ Lens.over synonyms (const syn)

    secondM f (x, y) = do
      y' <- f y
      return (x, y')

wrapperEnv :: Identifier -> (Kind, Maybe Wrapper) -> Infer Env
wrapperEnv n (_, w) =
  case w of
    Nothing -> return mempty
    Just (Wrapper vs t@(_, loc) maker getter) ->
      let
        wrapped = vs
          & map (\v -> (D.TVar v, loc))
          & foldl (\t1 t2 -> (D.TCall t1 t2, loc)) (D.TCon n, loc)
      in
      Error.defContext n do
        unwrapped <- unaliasType t
        checkKind KType wrapped
        checkKind KType unwrapped
        let wrapped' = convertType wrapped
        let unwrapped' = convertType unwrapped
        makerType <- Forall vs <$> functionType loc unwrapped' wrapped'
        getterType <- Forall vs <$> functionType loc wrapped' unwrapped'
        let makerEnv = Map.singleton maker makerType
        let typeEnv = case getter of
              Nothing -> makerEnv
              Just gt -> Map.insert gt getterType makerEnv
        return (Env typeEnv Map.empty Map.empty Map.empty)

checkModule :: Module -> Either Error.Msg ()
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

    checkForeign n (D.Forall _ t) =
      Error.defContext n $ Monad.void (checkKind KType t)

    check = do
      syns' <- synonymMap syns
      Reader.local (Lens.over synonyms $ const syns') do
        foreigns' <- sequence (Map.mapWithKey unaliasForeign foreigns)
        let foreignE = Env (fmap convertScheme foreigns') Map.empty Map.empty Map.empty
        wrapperE <- Fold.fold <$> sequence (Map.mapWithKey wrapperEnv types)
        Reader.local ((foreignE <> wrapperE) <>) do
          sequence_ (Map.mapWithKey checkForeign foreigns')
          checkFuncs funcs
