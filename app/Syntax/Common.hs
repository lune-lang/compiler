module Syntax.Common
  ( Line
  , Column
  , Position(..)
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

data Position
  = Fake Int
  | Real Int Line Column
  deriving (Eq)

type Location = (FilePath, Position)

type Name = String
type Label = String
type ModName = String

data Identifier
  = Qualified ModName Name
  | Unqualified Name
  deriving (Eq, Ord)

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
  | LazyType
  | DelayFunction
  | CurlyBrackets
  | SquareBrackets
  | RowConstructor
  deriving (Eq, Ord)

opChars :: [Char]
opChars = "~!@#$%^&*-+=|\\:;<>?./"
