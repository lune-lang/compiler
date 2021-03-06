module Syntax.Frontend
  ( SimpleExpr(..)
  , Expr
  , SimpleType(..)
  , Type
  , Scheme(..)
  , Wrapper(..)
  , SimpleLocalDef(..)
  , LocalDef
  , WithText
  , SimpleDef(..)
  , Def
  , SimplePort(..)
  , Assoc(..)
  , OpTable
  , Port
  , SimpleImport(..)
  , Import
  , Module(..)
  ) where

import Data.Map (Map)

import Syntax.Common

data SimpleExpr
  = Int Int
  | Float Double
  | Char Char
  | String String
  | Label Label
  | Identifier Identifier
  | Negate Expr
  | Operator Name (Maybe Expr) (Maybe Expr)
  | DefIn [LocalDef] Expr
  | Lambda [Name] Expr
  | Delay Expr
  | Call Expr Expr

type Expr = (SimpleExpr, Location)

data SimpleType
  = TCon Identifier
  | TOperator Name Type Type
  | TLabel Label
  | TRecord Type
  | TVariant Type
  | TCall Type Type

type Type = (SimpleType, Location)

data Scheme = Forall [Name] Type

data Wrapper = Wrapper [Name] (WithText Type) (Name, Location) (Maybe (Name, Location))

data SimpleLocalDef
  = LAnnotation [Name] Scheme
  | LFunc Name [Name] Expr

type LocalDef = (SimpleLocalDef, Location)

type WithText a = (a, String)

data SimpleDef
  = Annotation [Name] (WithText Scheme)
  | Foreign [Name] (WithText Scheme) [Identifier]
  | Func Name [Name] Expr
  | Expand Name [Name] (WithText Expr)
  | Type Name (WithText Kind) (Maybe Wrapper)
  | Synonym Name [Name] (WithText Type)
  | Infix Name Assoc Int
  | Syntax Name Role
  | Documentation String

type Def = (SimpleDef, Location)

data Assoc = LeftAssoc | RightAssoc | NonAssoc

type OpTable = Map Name (Assoc, Int)

data SimplePort
  = ValuePort Name
  | TypePort Name
  deriving (Eq)

type Port = (SimplePort, Location)

data SimpleImport
  = Import ModName (Maybe Name) (Maybe [Port])
  | ImportOpen ModName

type Import = (SimpleImport, Location)

data Module = Module
  { getExports :: [Port]
  , getImports :: [Import]
  , getDefs :: [Def]
  }
