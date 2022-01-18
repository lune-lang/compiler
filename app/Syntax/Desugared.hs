{-# LANGUAGE LambdaCase #-}

module Syntax.Desugared
  ( SimpleExpr(..)
  , Expr
  , SimpleType(..)
  , Type
  , Wrapper(..)
  , Module(..)
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)

import Syntax.Common

data SimpleExpr
  = Int Int
  | Float Double
  | Char Char
  | String String
  | Label Label
  | Identifier Identifier
  | DefIn Name (Maybe Type) Expr Expr
  | Lambda Name (Maybe Type) Expr
  | Call Expr Expr
  | Annotate Type

type Expr = (SimpleExpr, Location)

data SimpleType
  = TCon Identifier
  | TVar Name
  | TLabel Label
  | TCall Type Type
  | TAny Name Type
  deriving (Eq)

type Type = (SimpleType, Location)

data Wrapper = Wrapper [Name] Type Identifier (Maybe Identifier)

data Module = Module
  { getFuncs :: [(Identifier, Maybe Type, Expr, Location)]
  , getExpands :: [(Identifier, [Name], Expr, Location)]
  , getForeigns :: Map Identifier (Type, Set Identifier)
  , getTypes :: Map Identifier (Kind, Maybe Wrapper)
  , getSynonyms :: [(Identifier, [Name], Type, Location)]
  , getSyntax :: Map Role (Either Type Expr)
  }

instance Semigroup Module where
  Module f1 e1 g1 t1 s1 x1 <> Module f2 e2 g2 t2 s2 x2 =
    Module (f1 ++ f2) (e1 ++ e2) (g1 <> g2) (t1 <> t2) (s1 ++ s2) (x1 <> x2)

instance Monoid Module where
  mempty = Module [] [] Map.empty Map.empty [] Map.empty
