{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}

module Compiler.Propagate (propagateModule) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Lens (_1, _2, _3, (%~))

import Syntax.Common
import Syntax.Desugared

forall :: [(Name, Location)] -> Type -> Type
forall vars t =
  case vars of
    [] -> t
    (var, loc) : rest -> (TAny var $ forall rest t, loc)

unforall :: Type -> ([(Name, Location)], Type)
unforall (tipe, loc) =
  case tipe of
    TAny var t -> Bf.first ((var, loc):) (unforall t)
    _ -> ([], (tipe, loc))

freeVars :: Type -> [Name]
freeVars (tipe, _) =
  case tipe of
    TCon _ -> []
    TVar var -> [var]
    TLabel _ -> []
    TCall t1 t2 -> freeVars t1 ++ freeVars t2
    TAny var t -> List.delete var (freeVars t)

varPositions :: Type -> Map Name Int
varPositions t = Map.fromList $ zip (freeVars t) [0..]

filterSort :: (Ord b) => (a -> Maybe b) -> [a] -> [a]
filterSort f = let
  pair x = (x, ) <$> f x
  in map fst . List.sortOn snd . Maybe.mapMaybe pair

normalise :: Type -> Type
normalise tipe =
  case unforall tipe of
    (vars, t) -> let
      position var = Map.lookup (fst var) (varPositions t)
      in forall (filterSort position vars) t

normaliseFunc
  :: (Identifier, Maybe Type, Expr, Location)
  -> (Identifier, Maybe Type, Expr, Location)
normaliseFunc = _2 %~ fmap normalise

normaliseWrapper :: Wrapper -> Wrapper
normaliseWrapper (Wrapper name t maker getter) =
  Wrapper name (normalise t) maker getter

normaliseTypeDef :: (Kind, Maybe Wrapper) -> (Kind, Maybe Wrapper)
normaliseTypeDef = _2 %~ fmap normaliseWrapper

annotate :: (?arr :: Maybe SimpleType) => Type -> Expr -> Expr
annotate tipe (expr, loc) =
  (, loc) case expr of
    DefIn name anno x1 x2 ->
      DefIn name anno x1 (annotate tipe x2)

    Lambda arg _ x
      | Just arr <- ?arr
      , (TCall (TCall (a, _) t1, _) t2, _) <- tipe
      , arr == a
      -> Lambda arg (Just t1) (annotate t2 x)

    _ -> Annotate (expr, loc) tipe

propagateExpr :: (?arr :: Maybe SimpleType) => Expr -> Expr
propagateExpr (expr, loc) =
  (, loc) case expr of
    DefIn name (Just tipe) x1 x2 ->
      DefIn name Nothing (propagateExpr x1)
        $ propagateExpr (annotate tipe x2)

    DefIn name Nothing x1 x2 ->
      DefIn name Nothing (propagateExpr x1) (propagateExpr x2)

    Lambda arg anno x -> Lambda arg anno (propagateExpr x)
    Call x1 x2 -> Call (propagateExpr x1) (propagateExpr x2)
    Annotate x tipe -> Annotate (propagateExpr x) tipe
    _ -> expr

propagateFunc
  :: (?arr :: Maybe SimpleType)
  => (Identifier, Maybe Type, Expr, Location)
  -> (Identifier, Maybe Type, Expr, Location)
propagateFunc (name, anno, x, loc) = let
  x' = case anno of
    Nothing -> propagateExpr x
    Just tipe -> propagateExpr (annotate tipe x)
  in (name, Nothing, x', loc)

joinLeft :: Maybe (Either a b) -> Maybe a
joinLeft = \case
  Just (Left x) -> Just x
  _ -> Nothing

propagateModule :: Module ->  Module
propagateModule (Module funcs expands foreigns types synonyms syntax) =
  let ?arr = fst <$> joinLeft (Map.lookup FunctionType syntax) in let
  funcs' = map (propagateFunc . normaliseFunc) funcs
  foreigns' = fmap (_1 %~ normalise) foreigns
  types' = fmap normaliseTypeDef types
  synonyms' = map (_3 %~ normalise) synonyms
  in Module funcs' expands foreigns' types' synonyms' syntax
