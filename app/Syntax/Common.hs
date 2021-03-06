module Syntax.Common
  ( Line
  , Column
  , Location
  , Name
  , Label
  , ModName
  , Identifier(..)
  , Kind(..)
  , Role(..)
  , opChars
  ) where

type Line = Int
type Column = Int
type Location = (FilePath, Line, Column)

type Name = String
type Label = String
type ModName = String

data Identifier
  = Qualified ModName Name
  | Unqualified Name
  deriving (Eq, Ord, Show)

data Kind
  = KType
  | KNum
  | KRow
  | KLabel
  | KArr Kind Kind
  deriving (Eq)

data Role
  = NegateFunction
  | FunctionType
  | NumberType
  | FloatType
  | CharType
  | StringType
  | LabelType
  | CurlyBrackets
  | SquareBrackets
  | RowConstructor
  deriving (Eq, Ord)

opChars :: [Char]
opChars = "~!@#$%^&*-+=|\\:;<>?./"
