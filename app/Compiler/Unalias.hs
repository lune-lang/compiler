{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Compiler.Unalias (unaliasModule) where

import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Fold
import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader)

import qualified Compiler.Arrange as Arrange
import qualified Compiler.Error as Error
import Syntax.Common
import Syntax.Desugared

type Expands = Map Identifier ([Name], Expr)
type Synonyms = Map Identifier ([Name], Type)
type Unalias = ExceptT Error.Msg (Reader (Expands, Synonyms))

applyExpr :: Map Name Expr -> Expr -> Expr
applyExpr subst expr@(_, loc) =
  case fst expr of
    Int _ -> expr
    Float _ -> expr
    Char _ -> expr
    String _ -> expr
    Label _ -> expr

    Identifier (Qualified _ _) -> expr
    Identifier (Unqualified var) ->
      Maybe.fromMaybe expr (Map.lookup var subst)

    DefIn name tipe x1 x2 -> let
      subst' = Map.delete name subst
      x1' = applyExpr subst' x1
      x2' = applyExpr subst' x2
      in (DefIn name tipe x1' x2', loc)

    Lambda arg x -> let
      subst' = Map.delete arg subst
      x' = applyExpr subst' x
      in (Lambda arg x', loc)

    Call x1 x2 -> let
      x1' = applyExpr subst x1
      x2' = applyExpr subst x2
      in (Call x1' x2', loc)

applyType :: Map Name Type -> Type -> Type
applyType subst tipe@(_, loc) =
  case fst tipe of
    TCon _ -> tipe
    TLabel _ -> tipe
    TVar var -> Maybe.fromMaybe tipe (Map.lookup var subst)
    TCall t1 t2 -> let
      t1' = applyType subst t1
      t2' = applyType subst t2
      in (TCall t1' t2', loc)

getExprFunc :: Expr -> Maybe (Identifier, Location)
getExprFunc (expr, loc) =
  case expr of
    Identifier name -> Just (name, loc)
    Call x1 _ -> getExprFunc x1
    _ -> Nothing

getTypeFunc :: Type -> Maybe (Identifier, Location)
getTypeFunc (tipe, loc) =
  case tipe of
    TCon name -> Just (name, loc)
    TCall t1 _ -> getTypeFunc t1
    _ -> Nothing

getExprArgs :: Expr -> [Expr]
getExprArgs (expr, _) =
  case expr of
    Call x1 x2 -> getExprArgs x1 ++ [x2]
    _ -> []

getTypeArgs :: Type -> [Type]
getTypeArgs (tipe, _) =
  case tipe of
    TCall t1 t2 -> getTypeArgs t1 ++ [t2]
    _ -> []

continueExpr :: Expr -> Unalias Expr
continueExpr expr@(_, loc) =
  case fst expr of
    DefIn name tipe x1 x2 -> do
      x1' <- unaliasExpr x1
      x2' <- unaliasExpr x2
      return (DefIn name tipe x1' x2', loc)

    Lambda arg x -> do
      x' <- unaliasExpr x
      return (Lambda arg x', loc)

    Call x1 x2 -> do
      x1' <- unaliasExpr x1
      x2' <- unaliasExpr x2
      return (Call x1' x2', loc)

    _ -> return expr

continueType :: Type -> Unalias Type
continueType tipe@(_, loc) =
  case fst tipe of
    TCall t1 t2 -> do
      t1' <- unaliasType t1
      t2' <- unaliasType t2
      return (TCall t1' t2', loc)

    _ -> return tipe

unaliasExpr :: Expr -> Unalias Expr
unaliasExpr expr = do
  let args = getExprArgs expr
  expands <- Reader.asks fst
  case getExprFunc expr of
    Nothing -> continueExpr expr
    Just (name, loc) ->
      case Map.lookup name expands of
        Nothing -> continueExpr expr
        Just (vars, replacement) ->
          case compare (length vars) (length args) of
            LT -> continueExpr expr
            GT -> Error.withLocation loc (Error.partialExpand name)
            EQ -> do
              args' <- mapM unaliasExpr args
              return $ applyExpr (Map.fromList $ zip vars args') replacement

unaliasType :: Type -> Unalias Type
unaliasType tipe = do
  let args = getTypeArgs tipe
  synonyms <- Reader.asks snd
  case getTypeFunc tipe of
    Nothing -> continueType tipe
    Just (name, loc) ->
      case Map.lookup name synonyms of
        Nothing -> continueType tipe
        Just (vars, replacement) ->
          case compare (length vars) (length args) of
            LT -> continueType tipe
            GT -> Error.withLocation loc (Error.partialSynonym name)
            EQ -> do
              args' <- mapM unaliasType args
              return $ applyType (Map.fromList $ zip vars args') replacement

unaliasScheme :: Scheme -> Unalias Scheme
unaliasScheme (Forall vars tipe) = Forall vars <$> unaliasType tipe

unaliasFunc :: (Identifier, Maybe Scheme, Expr, Location) -> Unalias (Identifier, Maybe Scheme, Expr, Location)
unaliasFunc (name, tipe, body, loc) =
  Error.annoContext [name] do
    tipe' <- mapM unaliasScheme tipe
    body' <- unaliasExpr body
    return (name, tipe', body', loc)

unaliasForeign :: Identifier -> Scheme -> Unalias Scheme
unaliasForeign name tipe =
  Error.annoContext [name] (unaliasScheme tipe)

unaliasWrapper :: Wrapper -> Unalias Wrapper
unaliasWrapper (Wrapper vars tipe maker getter) = do
  tipe' <- unaliasType tipe
  return (Wrapper vars tipe' maker getter)

unaliasTypeDef :: Identifier -> (Kind, Maybe Wrapper) -> Unalias (Kind, Maybe Wrapper)
unaliasTypeDef name (kind, wrapper) =
  Error.defContext name do
    wrapper' <- mapM unaliasWrapper wrapper
    return (kind, wrapper')

unaliasSyntax :: Either Type Expr -> Unalias (Either Type Expr)
unaliasSyntax = \case
  Left tipe -> Left <$> unaliasType tipe
  Right expr -> Right <$> unaliasExpr expr

expandMap :: [(Identifier, [Name], Expr, Location)] -> Unalias Expands
expandMap = Fold.foldrM addExpand Map.empty

addExpand :: (Identifier, [Name], Expr, Location) -> Expands -> Unalias Expands
addExpand (name, vars, expr, _) expands =
  Error.defContext name do
    let exd = Map.singleton name (vars, expr)
    let withExp = Reader.local $ Bf.first (const exd)
    let secondM f (x, y) = (x, ) <$> f y
    expands' <- withExp $ mapM (secondM unaliasExpr) expands
    return (exd <> expands')

synonymMap :: [(Identifier, [Name], Type, Location)] -> Unalias Synonyms
synonymMap = Fold.foldrM addSynonym Map.empty

addSynonym :: (Identifier, [Name], Type, Location) -> Synonyms -> Unalias Synonyms
addSynonym (name, vars, tipe, _) synonyms =
  Error.defContext name do
    let syn = Map.singleton name (vars, tipe)
    let withSyn = Reader.local $ Bf.second (const syn)
    let secondM f (x, y) = (x, ) <$> f y
    synonyms' <- withSyn $ mapM (secondM unaliasType) synonyms
    return (syn <> synonyms')

unalias :: Module -> Unalias Module
unalias (Module funcs expands foreigns types synonyms syntax) = do
  expands' <- expandMap =<< Arrange.arrange False expands
  synonyms' <- synonymMap =<< Arrange.arrange False synonyms
  Reader.local (const (expands', synonyms')) do
    funcs' <- Arrange.arrange True =<< mapM unaliasFunc funcs
    foreigns' <- sequence (Map.mapWithKey unaliasForeign foreigns)
    types' <- sequence (Map.mapWithKey unaliasTypeDef types)
    syntax' <- mapM unaliasSyntax syntax
    return (Module funcs' expands foreigns' types' synonyms syntax')

unaliasModule :: Module -> Either Error.Msg Module
unaliasModule m = Reader.runReader (Except.runExceptT $ unalias m) (Map.empty, Map.empty)
