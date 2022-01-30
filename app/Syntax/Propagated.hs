module Syntax.Propagated
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

import qualified Syntax.Desugared as D
import Syntax.Common

data SimpleExpr
  = Literal Literal
  | Identifier Identifier
  | DefIn Name Expr Expr
  | Lambda Name (Maybe D.Type) Expr
  | Call Expr Expr

type Expr = (SimpleExpr, Bool, Location)

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
  { getFuncs :: [(Identifier, Expr, Location)]
  , getExpands :: [(Identifier, [Name], Expr, Location)]
  , getForeigns :: Map Identifier (D.Type, Set Identifier)
  , getTypes :: Map Identifier (Kind, Maybe D.Wrapper)
  , getSynonyms :: [(Identifier, [Name], D.Type, Location)]
  , getSyntax :: Map Role (Either D.Type Expr)
  }

instance Semigroup Module where
  Module f1 e1 g1 t1 s1 x1 <> Module f2 e2 g2 t2 s2 x2 =
    Module (f1 ++ f2) (e1 ++ e2) (g1 <> g2) (t1 <> t2) (s1 ++ s2) (x1 <> x2)

instance Monoid Module where
  mempty = Module [] [] Map.empty Map.empty [] Map.empty
