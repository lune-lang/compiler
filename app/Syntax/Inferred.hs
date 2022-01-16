{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Syntax.Inferred
  ( Origin(..)
  , Type(.., TCall2, TCall3)
  , escape
  ) where

import Syntax.Common

data Origin = Annotated Name | Inferred
  deriving (Eq)

data Type
  = TCon Identifier
  | TVar (Name, Origin)
  | TLabel Label
  | TCall Type Type
  | TAny Name Type
  deriving (Eq)

escape :: Type -> Type
escape = \case
  TAny _ t -> escape t
  t -> t

pattern TCall2 :: Type -> Type -> Type -> Type
pattern TCall2 t1 t2 t3 = TCall (TCall t1 t2) t3

pattern TCall3 :: Type -> Type -> Type -> Type -> Type
pattern TCall3 t1 t2 t3 t4 = TCall (TCall2 t1 t2 t3) t4
