{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Compiler.Unalias (unaliasModule) where

import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Fold

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader)

import qualified Compiler.Error as Error
import Syntax.Common
import Syntax.Desugared

type Synonyms = Map Identifier ([Name], Type)
type Unalias = ExceptT Error.Msg (Reader Synonyms)

apply :: Map Name Type -> Type -> Type
apply subst tipe@(_, loc) =
  case fst tipe of
    TCon _ -> tipe
    TLabel _ -> tipe
    TVar var -> Maybe.fromMaybe tipe (Map.lookup var subst)
    TCall t1 t2 -> let
      t1' = apply subst t1
      t2' = apply subst t2
      in (TCall t1' t2', loc)

getFunc :: Type -> Maybe (Identifier, Location)
getFunc (tipe, loc) =
  case tipe of
    TCon name -> Just (name, loc)
    TLabel _ -> Nothing
    TVar _ -> Nothing
    TCall t1 _ -> getFunc t1

getArgs :: Type -> [Type]
getArgs (tipe, _) =
  case tipe of
    TCon _ -> []
    TLabel _ -> []
    TVar _ -> []
    TCall t1 t2 -> getArgs t1 ++ [t2]

keepLooking :: Type -> Unalias Type
keepLooking tipe@(_, loc) =
  case fst tipe of
    TCon _ -> return tipe
    TLabel _ -> return tipe
    TVar _ -> return tipe
    TCall t1 t2 -> do
      t1' <- unaliasType t1
      t2' <- unaliasType t2
      return (TCall t1' t2', loc)

unaliasType :: Type -> Unalias Type
unaliasType tipe = do
  let args = getArgs tipe
  syns <- Reader.ask
  case getFunc tipe of
    Nothing -> keepLooking tipe
    Just (name, loc) ->
      case Map.lookup name syns of
        Nothing -> keepLooking tipe
        Just (vars, replacement) ->
          case compare (length vars) (length args) of
            LT -> keepLooking tipe
            GT -> Error.withLocation loc (Error.partialApplication name)
            EQ -> do
              args' <- mapM unaliasType args
              return $ apply (Map.fromList $ zip vars args') replacement

unaliasScheme :: Scheme -> Unalias Scheme
unaliasScheme (Forall vars tipe) = Forall vars <$> unaliasType tipe

unaliasFunc :: (Identifier, Maybe Scheme, Expr, Location) -> Unalias (Identifier, Maybe Scheme, Expr, Location)
unaliasFunc (name, tipe, body, loc) =
  Error.annoContext [name] do
    tipe' <- mapM unaliasScheme tipe
    return (name, tipe', body, loc)

unaliasForeign :: Identifier -> (Scheme, String) -> Unalias (Scheme, String)
unaliasForeign name (tipe, js) =
  Error.annoContext [name] do
    tipe' <- unaliasScheme tipe
    return (tipe', js)

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
  Right expr -> return (Right expr)

synonymMap :: [(Identifier, [Name], Type, Location)] -> Unalias Synonyms
synonymMap = Fold.foldrM addSynonym Map.empty

addSynonym :: (Identifier, [Name], Type, Location) -> Synonyms -> Unalias Synonyms
addSynonym (name, vars, tipe, _) syns =
  Error.defContext name do
    let syn = Map.singleton name (vars, tipe)
    let withSyn = Reader.local (const syn)
    let secondM f (x, y) = (x, ) <$> f y
    syns' <- withSyn $ mapM (secondM unaliasType) syns
    return (syn <> syns')

unalias :: Module -> Unalias Module
unalias (Module funcs foreigns types syns syntax) = do
  syns' <- synonymMap syns
  Reader.local (const syns') do
    funcs' <- mapM unaliasFunc funcs
    foreigns' <- sequence (Map.mapWithKey unaliasForeign foreigns)
    types' <- sequence (Map.mapWithKey unaliasTypeDef types)
    syntax' <- mapM unaliasSyntax syntax
    return (Module funcs' foreigns' types' syns syntax')

unaliasModule :: Module -> Either Error.Msg Module
unaliasModule m = Reader.runReader (Except.runExceptT $ unalias m) Map.empty
