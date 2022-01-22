{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}

module Compiler.Propagate (propagateModule) where

import qualified Data.Map as Map

import Syntax.Common
import Syntax.Desugared

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
  let ?arr = fst <$> joinLeft (Map.lookup FunctionType syntax) in
  let funcs' = map propagateFunc funcs in
  Module funcs' expands foreigns types synonyms syntax
