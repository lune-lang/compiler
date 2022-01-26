module Syntax.Frontend
  ( SimpleExpr(..)
  , Expr
  , SimpleType(..)
  , Type
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
  = Literal Literal
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
  | TAny [Name] Type

type Type = (SimpleType, Location)

data Wrapper = Wrapper [Name] (WithText Type) (Name, Location) (Maybe (Name, Location))

data SimpleLocalDef
  = LAnnotation [Name] Type
  | LFunc Name [Name] Expr

type LocalDef = (SimpleLocalDef, Location)

type WithText a = (a, String)

data SimpleDef
  = Annotation [Name] (WithText Type)
  | Foreign [Name] (WithText Type) [Identifier]
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
