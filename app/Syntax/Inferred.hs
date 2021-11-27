module Syntax.Inferred
  ( Origin(..)
  , Type(..)
  , Scheme(..)
  ) where

import Syntax.Common

data Origin = Annotated | Inferred
  deriving (Eq)

data Type
  = TCon Identifier
  | TVar (Name, Origin)
  | TLabel Label
  | TCall Type Type
  deriving (Eq)

data Scheme = Forall [Name] Type
