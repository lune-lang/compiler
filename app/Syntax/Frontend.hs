module Syntax.Frontend
  ( Expr(..)
  , Type(..)
  , Scheme(..)
  , Wrapper(..)
  , LocalDef(..)
  , Def(..)
  , Port(..)
  , Import(..)
  , Module(..)
  ) where

import qualified Control.Lens as Lens

import Syntax.Common

data Expr
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
  | Call Expr Expr

data Type
  = TCon Identifier
  | TOperator Name Type Type
  | TLabel Label
  | TRecord Type
  | TVariant Type
  | TCall Type Type

data Scheme = Forall [Name] Type

data Wrapper = Wrapper [Name] Type Name (Maybe Name)

data LocalDef
  = LAnnotation [Name] Scheme
  | LFunc Name [Name] Expr

data Def
  = Annotation [Name] Scheme
  | Func Name [Name] Expr
  | Foreign Name [Name] String
  | Type Name Kind (Maybe Wrapper)
  | Synonym Name [Name] Type
  | Infix Name
  | Syntax Name Role

data Port
  = ValuePort Name
  | TypePort Name
  deriving (Eq)

data Import
  = Import ModName (Maybe Name) (Maybe [Port])
  | ImportOpen ModName

data Module = Module
  { getExports :: [Port]
  , getImports :: [Import]
  , getDefs :: [Def]
  }
