{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Infer (checkModule) where

import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bf
import qualified Data.Foldable as Fold
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
import Control.Lens ((^.), (.~), (%~))

import qualified Compiler.Error as Error
import qualified Syntax.Desugared as D
import Syntax.Common
import Syntax.Desugared (SimpleExpr(..), Expr, Wrapper(..), Module(..))
import Syntax.Inferred

convertType :: D.Type -> Type
convertType (tipe, _) =
  case tipe of
    D.TCon n -> TCon n
    D.TVar n -> TVar (n, Annotated n)
    D.TLabel n -> TLabel n
    D.TCall t1 t2 -> TCall (convertType t1) (convertType t2)
    D.TAny n t -> TAny n (convertType t)

data Env = Env
  { _typeEnv :: Map Identifier Type
  , _kindEnv :: Map Identifier Kind
  , _syntax :: Map Role (Either Type Expr)
  }

Lens.makeLenses ''Env

instance Semigroup Env where
  Env t1 k1 s1 <> Env t2 k2 s2 =
    Env (t1 <> t2) (k1 <> k2) (s1 <> s2)

instance Monoid Env where
  mempty = Env Map.empty Map.empty Map.empty

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
    TAny n t -> TAny n $ apply (Map.delete n s) t

  freeVars = \case
    TCon _ -> Set.empty
    TLabel _ -> Set.empty
    TVar (n, _) -> Set.singleton n
    TCall t1 t2 -> freeVars t1 <> freeVars t2
    TAny n t -> Set.delete n (freeVars t)

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

extendEnv :: Identifier -> Type -> Infer a -> Infer a
extendEnv n t = Reader.local (typeEnv %~ Map.insert n t)

applyEnv :: Subst -> Infer a -> Infer a
applyEnv s = Reader.local (typeEnv %~ apply s)

specialType :: Role -> Location -> InferWith String Type -> Infer Type
specialType role loc err = do
  special <- Reader.asks (^. syntax)
  case Map.lookup role special of
    Just (Left t) -> return t
    _ -> Error.withLocation loc err

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

rowCons :: Infer (Maybe Type)
rowCons = do
  special <- Reader.asks (^. syntax)
  case Map.lookup RowConstructor special of
    Just (Left t) -> return (Just t)
    _ -> return Nothing

freshVar :: Origin -> Infer Type
freshVar origin = do
  var <- State.gets (first . Lens.view freshNames)
  State.modify (freshNames %~ rest)
  return $ TVar (var, origin)
  where
    first (Cons n _) = n
    rest (Cons _ ns) = ns

generalise :: Type -> Infer Type
generalise t = do
  env <- Reader.asks (^. typeEnv)
  let vars = freeVars t \\ freeVars env
  return (foldr TAny t vars)

inferType :: Expr -> Infer (Subst, Type)
inferType (expr, loc) =
  case expr of
    Int _ -> basic (numberType loc =<< freshVar Inferred)
    Float _ -> basic (floatType loc)
    Char _ -> basic (charType loc)
    String _ -> basic (stringType loc)
    Label n -> basic (labelType loc n)

    Identifier n -> do
      env <- Reader.asks (^. typeEnv)
      case Map.lookup n env of
        Nothing -> Error.withLocation loc (Error.notDefined n)
        Just t -> return (nullSubst, t)

    --DefIn

    Lambda n x -> do
      var <- freshVar Inferred
      (s, t) <- extendEnv (Unqualified n) var (inferType x)
      tf <- apply s <$> functionType loc var (escape t)
      applyEnv s $ (s, ) <$> generalise tf

    {-
    Lambda n x -> do
      var <- freshVar Inferred
      t <- extendEnv (Unqualified n) (Forall [] var) (inferType x)
      functionType loc var t
    -}

    _ -> undefined
    where
      basic f = (nullSubst, ) <$> f


{-
instantiate :: (Name -> Origin) -> Type -> Infer Type
instantiate origin (TAny vs t) = do
  vs' <- mapM (freshVar . origin) vs
  let s = Map.fromList $ zip vs vs'
  return (apply s t)

generalise :: Type -> Infer Type
generalise t = do
  env <- Reader.asks (^. typeEnv)
  let vs = freeVars t \\ freeVars env
  return $ Forall (Set.toList vs) t

unify :: Location -> Type -> Type -> Infer ()
unify loc t1 t2 = State.modify $ constraints %~ ((loc, t1, t2):)

inferType :: Expr -> Infer Type
inferType (expr, loc) =
  case expr of
    Int _ -> freshVar Inferred >>= numberType loc
    Float _ -> floatType loc
    Char _ -> charType loc
    String _ -> stringType loc
    Label n -> labelType loc n

    Identifier n -> do
      env <- Reader.asks (^. typeEnv)
      maybe (Error.withLocation loc $ Error.notDefined n)
        (instantiate $ const Inferred) (Map.lookup n env)

    DefIn n maybeAnno x1@(_, loc1) x2 ->
      let
        getSubst t =
          case maybeAnno of
            Nothing -> solve
            Just anno@(D.TAny _ (_, annoLoc)) -> do
              anno' <- instantiate Annotated (convertScheme anno)
              unify loc1 t anno'
              s <- solve
              equivalent annoLoc anno' (apply s anno')
              return s

        getType = do
          var <- freshVar Inferred
          t <- extendEnv (Unqualified n) (Forall [] var) (inferType x1)
          unify loc1 t var
          s <- getSubst t
          return (s, t)
      in do
      (s, t) <- getType
      Reader.local (typeEnv %~ apply s) do
        sc <- generalise (apply s t)
        extendEnv (Unqualified n) sc (inferType x2)

    Lambda n x -> do
      var <- freshVar Inferred
      t <- extendEnv (Unqualified n) (Forall [] var) (inferType x)
      functionType loc var t

    Call x1@(_, loc1) x2@(_, loc2) -> do
      t1 <- inferType x1
      t2 <- inferType x2
      var <- freshVar Inferred
      tf <- functionType loc1 t2 var
      unify loc2 tf t1
      return var

unifies :: Maybe Type -> Location -> Type -> Type -> Infer Subst
unifies cons loc = curry \case
  (t1, t2) | t1 == t2 -> return nullSubst

  (TVar n, t) -> bind loc n t
  (t, TVar n) -> bind loc n t

  (TCall3 n l v1 r1, t2) | Just cons' <- cons, n == cons' -> do
      (v2, r2) <- rowGet cons' loc l t2
      s1 <- unifies cons loc v1 v2
      s2 <- unifies cons loc (apply s1 r1) (apply s1 r2)
      return (compose s2 s1)

  (TCall t1 t2, TCall t3 t4) -> do
    s1 <- unifies cons loc t1 t3
    s2 <- unifies cons loc (apply s1 t2) (apply s1 t4)
    return (compose s2 s1)

  (t1, t2) -> Error.withLocation loc (Error.unification t1 t2)

rowGet :: Type -> Location -> Type -> Type -> Infer (Type, Type)
rowGet cons loc label = \case
  (TCall3 n l v r) | n == cons ->
    if l == label then return (v, r)
    else Bf.second (TCall3 n l v) <$> rowGet cons loc label r

  TVar (_, origin) -> (,) <$> freshVar origin <*> freshVar origin

  t -> Error.withLocation loc (Error.noLabel label t)

bind :: Location -> (Name, Origin) -> Type -> Infer Subst
bind loc (n, origin) t =
  case origin of
    Annotated n' | not variable ->
      Error.withLocation loc (Error.generalAnno n' t)
    _ | Set.member n (freeVars t) ->
      Error.withLocation loc (Error.occursCheck n t)
    _ -> return (Map.singleton n t)
  where
    variable = case t of
      TVar _ -> True
      _ -> False

solver :: Subst -> [Constraint] -> Infer Subst
solver s = \case
  [] -> return s
  (loc, t1, t2) : cs -> do
    cons <- rowCons
    s1 <- unifies cons loc t1 t2
    let sub = Bf.bimap (apply s1) (apply s1)
    solver (compose s1 s) (map sub cs)

solve :: Infer Subst
solve = State.gets (^. constraints) >>= solver nullSubst

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
      env <- Reader.asks (^. kindEnv)
      case Map.lookup n env of
        Nothing -> Error.withLocation loc (Error.notDefined n)
        Just k -> return (k, Map.empty)

    D.TLabel _ -> return (KLabel, Map.empty)
    D.TVar _ -> Error.withLocation loc Error.typeVariables

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

checkFuncs :: [(Identifier, Maybe D.Type, Expr, Location)] -> Infer ()
checkFuncs = \case
  [] -> return ()
  (n, maybeAnno, x@(_, loc), _) : fs ->
    let
      getSubst t =
        case maybeAnno of
          Nothing -> solve
          Just anno@(D.TAny _ (_, annoLoc)) -> do
            anno' <- instantiate Annotated (convertScheme anno)
            unify loc t anno'
            s <- solve
            equivalent annoLoc anno' (apply s anno')
            return s

      getType = do
        var <- freshVar Inferred
        t <- extendEnv n (Forall [] var) (inferType x)
        unify loc t var
        s <- getSubst t
        return (s, t)
    in do
    (s, t) <- Error.defContext n getType
    Reader.local (typeEnv %~ apply s) $ do
      sc <- generalise (apply s t)
      State.modify (constraints .~ [])
      extendEnv n sc (checkFuncs fs)

equivalent :: Location -> Type -> Type -> Infer ()
equivalent loc tipe1 tipe2 =
  Monad.when bad $ Error.withLocation loc (Error.unification tipe1 tipe2)
  where
    pairings = curry \case
      (TCall t1 t2, TCall t3 t4) -> pairings t1 t3 <> pairings t2 t4
      (TVar (v1, _), TVar (v2, _)) -> Set.singleton (v1, v2)
      _ -> Set.empty

    pairs = pairings tipe1 tipe2

    bad =
      Set.size (Set.map fst pairs) < Set.size pairs ||
      Set.size (Set.map snd pairs) < Set.size pairs

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
        _ <- checkKind KType wrapped
        _ <- checkKind KType t
        let wrapped' = convertType wrapped
        let t' = convertType t
        makerType <- Forall vs <$> functionType loc t' wrapped'
        getterType <- Forall vs <$> functionType loc wrapped' t'
        let envMaker = Map.singleton maker makerType
        let envTypes = case getter of
              Nothing -> envMaker
              Just gt -> Map.insert gt getterType envMaker
        return (Env envTypes Map.empty Map.empty)

checkModule :: Module -> Either Error.Msg ()
checkModule (Module funcs _ foreigns types _ syntaxDefs) =
  State.evalState (Reader.runReaderT (Except.runExceptT check) env) state
  where
    state = InferState [] (foldr Cons undefined fresh)

    envTypes = fmap (convertScheme . fst) foreigns
    envKinds = fmap fst types
    envSyntax = fmap (Bf.first convertType) syntaxDefs
    env = Env envTypes envKinds envSyntax

    prefixes = map (:[]) ['a'..'z']
    numbers = concatMap (replicate $ length prefixes) ([0..] :: [Int])
    fresh = zipWith (++) (cycle prefixes) (map show numbers)

    checkForeign n (D.TAny _ t, _) =
      Error.defContext n $ Monad.void (checkKind KType t)

    check = do
      wrapperE <- Fold.fold <$> sequence (Map.mapWithKey wrapperEnv types)
      Reader.local (wrapperE <>) do
        sequence_ (Map.mapWithKey checkForeign foreigns)
        checkFuncs funcs
-}

checkModule :: Module -> Either Error.Msg ()
checkModule = undefined
