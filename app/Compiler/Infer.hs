{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Infer (checkModule) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bf
import qualified Data.Foldable as Fold
import qualified Control.Monad as Monad
import Data.List ((\\))
import Data.Function ((&))

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)

import qualified Control.Lens as Lens
import Control.Lens ((^.), (%~))

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
    D.TVar n -> TVar n
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

infixr 9 `compose`
compose :: Subst -> Subst -> Subst
compose s2 s1 = s2 <> apply s2 s1

class Substitutable a where
  apply :: Subst -> a -> a
  freeVars :: a -> [Name]
  hasFree :: Name -> a -> Bool

instance Substitutable Type where
  apply s = \case
    TCon n -> TCon n
    TLabel n -> TLabel n
    TVar n -> Maybe.fromMaybe (TVar n) (Map.lookup n s)
    TCall t1 t2 -> TCall (apply s t1) (apply s t2)
    TAny n t -> TAny n $ apply (Map.delete n s) t

  freeVars = \case
    TCon _ -> []
    TLabel _ -> []
    TVar n -> [n]
    TCall t1 t2 -> freeVars t1 ++ freeVars t2
    TAny n t -> List.delete n (freeVars t)

  hasFree var = \case
    TCon _ -> False
    TLabel _ -> False
    TVar n -> n == var
    TCall t1 t2 -> hasFree var t1 || hasFree var t2
    TAny n t -> n /= var && hasFree var t

instance Substitutable a => Substitutable (Map k a) where
  apply s = fmap (apply s)
  freeVars = concatMap freeVars
  hasFree n = any (hasFree n)

data Fresh = Cons Name Fresh

type InferWith e = ExceptT e (ReaderT Env (State Fresh))
type Infer = InferWith Error.Msg

extendEnv :: Identifier -> Type -> Infer a -> Infer a
extendEnv n t = Reader.local (typeEnv %~ Map.insert n t)

applyEnv :: Subst -> Infer a -> Infer a
applyEnv s = Reader.local (typeEnv %~ apply s)

typeRole :: Role -> Infer (Maybe Type)
typeRole role = do
  special <- Reader.asks (^. syntax)
  case Map.lookup role special of
    Just (Left x) -> return (Just x)
    _ -> return Nothing

specialType :: Role -> Location -> InferWith String Type -> Infer Type
specialType role loc err =
  typeRole role >>=
  maybe (Error.withLocation loc err) return

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

freshName :: Infer Name
freshName = do
  var <- State.gets first
  State.modify rest
  return var
  where
    first (Cons n _) = n
    rest (Cons _ ns) = ns

freshVar :: Infer Type
freshVar = TVar <$> freshName

instantiate :: Type -> Infer Type
instantiate t = snd <$> unforallFresh t

unforallFresh :: Type -> Infer ([Name], Type)
unforallFresh tipe =
  case unforall tipe of
    (vs, t) -> do
      vs' <- mapM (const freshName) vs
      let s = Map.fromList $ zip vs (map TVar vs')
      return (vs', apply s t)

substFresh :: Type -> Infer Type
substFresh t = do
  let vs = freeVars t
  vs' <- mapM (const freshVar) vs
  let s = Map.fromList (zip vs vs')
  return (apply s t)

generalise :: Type -> Infer Type
generalise t = do
  env <- Reader.asks (^. typeEnv)
  let vars = freeVars t \\ freeVars env
  return (foldr TAny t vars)

inferType :: Expr -> Infer (Subst, Type)
inferType (expr, loc) =
  case expr of
    Int _ -> basic (numberType loc =<< freshVar)
    Float _ -> basic (floatType loc)
    Char _ -> basic (charType loc)
    String _ -> basic (stringType loc)
    Label n -> basic (labelType loc n)

    Identifier n -> do
      env <- Reader.asks (^. typeEnv)
      case Map.lookup n env of
        Nothing -> Error.withLocation loc (Error.notDefined n)
        Just t -> return (nullSubst, t)

    DefIn n _ x1@(_, loc1) x2 -> do
      {-
      (s1, t1) <- inferType x1
      (s2, t2) <- applyEnv s1 $ extendEnv (Unqualified n) t1 (inferType x2)
      return (s2 `compose` s1, t2)
      -}
      cons <- typeRole RowConstructor
      let ?cons = cons
      var <- freshVar
      (s1, t1) <- extendEnv (Unqualified n) var (inferType x1)
      s2 <- unify loc1 (apply s1 var) t1
      (s3, t2) <- applyEnv s2 do
        t1' <- generalise t1
        extendEnv (Unqualified n) t1' (inferType x2)
      return (s3 `compose` s2, t2)

    Lambda n Nothing x -> do
      var <- freshVar
      (s, t) <- extendEnv (Unqualified n) var (inferType x)
      Monad.when (polymorphic $ apply s var)
        (Error.withLocation loc Error.unsure)
      t' <- instantiate t
      tf <- apply s <$> functionType loc var t'
      applyEnv s $ (s, ) <$> generalise tf

    Lambda n (Just anno) x -> do
      anno' <- substFresh (convertType anno)
      (s, t) <- extendEnv (Unqualified n) anno' (inferType x)
      tf <- apply s <$> functionType loc anno' t
      applyEnv s $ (s, ) <$> generalise tf

    Call x1 x2 -> inferCall False x1 x2

    Annotate x t -> let
      fun = (Lambda "_" (Just t) (Identifier (Unqualified "_"), loc), loc)
      in traceShowM (convertType t) >> inferCall True fun x

    where
      basic f = (nullSubst, ) <$> f

inferCall :: Bool -> Expr -> Expr -> Infer (Subst, Type)
inferCall rigid x1@(_, loc1) x2@(_, loc2) = do
  arr <- typeRole FunctionType
  cons <- typeRole RowConstructor
  let ?cons = cons
  (s0, t1) <- inferType x1
  (s1, t1a, t1b) <- unifyArr arr loc1 =<< instantiate t1
  (s2, t2) <- applyEnv (s1 `compose` s0) (inferType x2)
  let uni = if rigid then unify else subsume
  s3 <- uni loc2 (apply s2 t1a) t2
  let s4 = s3 `compose` s2 `compose` s1 `compose` s0
  applyEnv s4 $ (s4, ) <$> generalise (apply s4 t1b)

unifyArr :: Maybe Type -> Location -> Type -> Infer (Subst, Type, Type)
unifyArr maybeArr loc = \case
  TCall2 a t1 t2 | Just arr <- maybeArr, arr == a ->
    return (nullSubst, t1, t2)

  TVar n -> do
    var1 <- freshVar
    var2 <- freshVar
    tf <- functionType loc var1 var2
    return (Map.singleton n tf, var1, var2)

  t -> do
    var1 <- freshVar
    var2 <- freshVar
    tf <- functionType loc var1 var2
    Error.withLocation loc (Error.unification t tf)

subsume :: (?cons :: Maybe Type) => Location -> Type -> Type -> Infer Subst
subsume loc t1 t2 = do
  (vars1, t1') <- unforallFresh t1
  (vars2, t2') <- unforallFresh t2
  s <- unify loc t1' t2'
  let s' = foldr Map.delete s vars2
  if null $ List.intersect vars1 (freeVars s')
    then return s'
    else Error.withLocation loc (Error.unification t1 t2)

unify :: (?cons :: Maybe Type) => Location -> Type -> Type -> Infer Subst
unify loc = curry \case
  (t1, t2) | t1 == t2 -> return nullSubst

  (TVar n, t) -> bind loc n t
  (t, TVar n) -> bind loc n t

  (TCall3 c l v1 r1, t2) | Just cons <- ?cons, c == cons -> do
    (s0, v2, r2) <- rowGet cons loc l t2
    s1 <- unify loc (apply s0 v1) (apply s0 v2)
    let s2 = s1 `compose` s0
    s3 <- unify loc (apply s2 r1) (apply s2 r2)
    return (s3 `compose` s2)

  (TCall t1 t2, TCall t3 t4) -> do
    s1 <- unify loc t1 t3
    s2 <- unify loc (apply s1 t2) (apply s1 t4)
    return (s2 `compose` s1)

  (TAny n1 t1, TAny n2 t2) -> do
    sk <- freshName
    let t1' = apply (Map.singleton n1 $ TVar sk) t1
    let t2' = apply (Map.singleton n2 $ TVar sk) t2
    s <- unify loc t1' t2'
    if hasFree sk s
      then Error.withLocation loc (Error.unification (TAny n1 t1) (TAny n2 t2))
      else return s

  (t1, t2) -> Error.withLocation loc (Error.unification t1 t2)

rowGet :: Type -> Location -> Type -> Type -> Infer (Subst, Type, Type)
rowGet cons loc label = \case
  (TCall3 c l v r) | c == cons ->
    if l == label then return (nullSubst, v, r)
    else do
      (s, v', r') <- rowGet cons loc label r
      return (s, v', TCall3 c l v r')

  TVar n -> do
    var1 <- freshVar
    var2 <- freshVar
    let t = TCall3 cons label var1 var2
    return (Map.singleton n t, var1, var2)

  t -> Error.withLocation loc (Error.noLabel label t)

bind :: Location -> Name -> Type -> Infer Subst
bind loc n t
  | hasFree n t =
      Error.withLocation loc (Error.occursCheck n t)
  | otherwise = return (Map.singleton n t)

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

    D.TAny n t -> do
      (k, s) <- inferKind t
      return (k, Map.delete n s)

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

  (n, _, x@(_, loc), _) : fs -> do
    {-
    (s, t) <- inferType x
    applyEnv s $ extendEnv n t (checkFuncs fs)
    -}
    cons <- typeRole RowConstructor
    let ?cons = cons
    var <- freshVar
    (s, t) <- extendEnv n var (inferType x)
    s2 <- unify loc (apply s var) t
    applyEnv s2 do
      t' <- generalise t
      traceM (show n ++ " :: " ++ show t')
      extendEnv n t' (checkFuncs fs)

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
        makerType <- forall vs <$> functionType loc t' wrapped'
        getterType <- forall vs <$> functionType loc wrapped' t'
        let envMaker = Map.singleton maker makerType
        let envTypes = case getter of
              Nothing -> envMaker
              Just gt -> Map.insert gt getterType envMaker
        return (Env envTypes Map.empty Map.empty)

checkModule :: Module -> Either Error.Msg ()
checkModule (Module funcs _ foreigns types _ syntaxDefs) =
  State.evalState (Reader.runReaderT (Except.runExceptT check) env) state
  where
    state = foldr Cons undefined fresh

    envTypes = fmap (convertType . fst) foreigns
    envKinds = fmap fst types
    envSyntax = fmap (Bf.first convertType) syntaxDefs
    env = Env envTypes envKinds envSyntax

    prefixes = map (:[]) ['a'..'z']
    numbers = concatMap (replicate $ length prefixes) ([0..] :: [Int])
    fresh = zipWith (++) (cycle prefixes) (map show numbers)

    checkForeign n (t, _) =
      Error.defContext n $ Monad.void (checkKind KType t)

    check = do
      wrapperE <- Fold.fold <$> sequence (Map.mapWithKey wrapperEnv types)
      Reader.local (wrapperE <>) do
        sequence_ (Map.mapWithKey checkForeign foreigns)
        checkFuncs funcs
