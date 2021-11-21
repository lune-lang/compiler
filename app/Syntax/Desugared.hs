{-# LANGUAGE LambdaCase #-}

module Syntax.Desugared
  ( SimpleExpr(..)
  , Expr
  , Origin(..)
  , TypeVar
  , SimpleType(..)
  , Type
  , Scheme(..)
  , Wrapper(..)
  , Module(..)
  ) where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Control.Lens as Lens

import Syntax.Common

data SimpleExpr
  = Int Int
  | Float Double
  | Char Char
  | String String
  | Label Label
  | Identifier Identifier
  | DefIn Name (Maybe Scheme) Expr Expr
  | Lambda Name Expr
  | Call Expr Expr

type Expr = (SimpleExpr, Location)

data Origin = Annotated | Inferred
  deriving (Eq)

type TypeVar = (Name, Origin)

data SimpleType
  = TCon Identifier
  | TVar TypeVar
  | TLabel Label
  | TCall Type Type
  deriving (Eq)

type Type = (SimpleType, Location)

data Scheme = Forall [Name] Type

data Wrapper = Wrapper [Name] Type Identifier (Maybe Identifier)

data Module = Module
  { getFuncs :: [(Identifier, Maybe Scheme, Expr)]
  , getForeigns :: Map Identifier (Scheme, String)
  , getTypes :: Map Identifier (Kind, Maybe Wrapper)
  , getSynonyms :: [(Identifier, [Name], Type)]
  , getSyntax :: Map Role Identifier
  }

instance Semigroup Module where
  Module f1 g1 t1 s1 x1 <> Module f2 g2 t2 s2 x2 =
    Module (f1 ++ f2) (g1 <> g2) (t1 <> t2) (s1 ++ s2) (x1 <> x2)

instance Monoid Module where
  mempty = Module [] Map.empty Map.empty [] Map.empty
