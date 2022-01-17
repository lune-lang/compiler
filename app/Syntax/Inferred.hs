{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Syntax.Inferred
  ( Origin(..)
  , Type(.., TCall2, TCall3)
  , forall, unforall, escape
  ) where

import qualified Data.Bifunctor as Bf

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

pattern TCall2 :: Type -> Type -> Type -> Type
pattern TCall2 t1 t2 t3 = TCall (TCall t1 t2) t3

pattern TCall3 :: Type -> Type -> Type -> Type -> Type
pattern TCall3 t1 t2 t3 t4 = TCall (TCall2 t1 t2 t3) t4

forall :: [Name] -> Type -> Type
forall ns t = foldr TAny t ns

unforall :: Type -> ([Name], Type)
unforall = \case
  TAny n t -> Bf.first (n:) (unforall t)
  t -> ([], t)

escape :: Type -> Type
escape = snd . unforall
