{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Error
  ( NameError
  , Msg
  , notDefined
  , multipleDef
  , exportFailure
  , multipleExport
  , importFailure
  , multipleInfix
  , conflictingSyntax
  , negateFunction
  , functionType
  , numberType
  , floatType
  , charType
  , stringType
  , labelType
  , curlyBrackets
  , squareBrackets
  , annotation
  , noForeignAnno
  , noModule
  , duplicateModule
  , noMainModule
  , noMainFunction
  , noSrcDirectory
  , directoryRead
  , mutualRecursion
  , synonymRecursion
  , partialExpand
  , partialSynonym
  , kindInference
  , kindCheck
  , typeVariables
  , generalAnno
  , occursCheck
  , typeCall
  , unification
  , kindUnification
  , noLabel
  , unsure
  , withLocation
  , defContext
  , annoContext
  ) where

import qualified Control.Monad.Except as Except
import Control.Monad.Except (ExceptT, MonadError)

import qualified Syntax.Frontend as F
import Syntax.Common
import Syntax.Inferred

{--
instance Show Identifier where
  show = \case
    Unqualified n -> parens (operator n) n
    Qualified m n -> m ++ "." ++ parens (operator n) n

instance Show Type where
  show = prettyType TypeOuter

instance Show Scheme where
  show (Forall vs t) = concat
    [ "any ", unwords vs, ". ", show t ]
--}

class NameError a where
  err :: a -> String

instance NameError Name where
  err n = concat [ "'", n, "'" ]

instance NameError Identifier where
  err = \case
    Unqualified n -> concat [ "'", parens (operator n) n, "'" ]
    Qualified m n -> concat [ "'", m, ".", parens (operator n) n, "'" ]

instance NameError F.SimplePort where
  err = \case
    F.ValuePort n -> err n
    F.TypePort n -> err n

newtype Msg = Msg String

instance Show Msg where
  show (Msg str) = str

notDefined :: (NameError a, MonadError String m) => a -> m b
notDefined n = Except.throwError $
  err n ++ " is not defined"

multipleDef :: (NameError a, MonadError String m) => a -> m b
multipleDef n = Except.throwError $
  err n ++ " is defined more than once"

exportFailure :: (NameError a, MonadError String m) => a -> m b
exportFailure n = Except.throwError $
  err n ++ " cannot be exported because it is not defined"

multipleExport :: (NameError a, MonadError String m) => a -> m b
multipleExport n = Except.throwError $
  err n ++ " is exported more than once"

importFailure :: (NameError a, MonadError String m) => a -> m b
importFailure n = Except.throwError
  $ err n ++ " cannot be imported because it is not defined"

multipleInfix :: (NameError a, MonadError String m) => a -> m b
multipleInfix n = Except.throwError $
  "multiple infix declarations for " ++ err n

conflictingSyntax :: (NameError a, MonadError String m) => a -> m b
conflictingSyntax n = Except.throwError $
  "syntax declaration for " ++ err n ++ " conflicts with others"

negateFunction :: (MonadError String m) => m b
negateFunction = Except.throwError
  "you cannot use the prefix operator without a negate-function declaration"

functionType :: (MonadError String m) => m b
functionType = Except.throwError
  "you cannot create functions without a function-type declaration"

numberType :: (MonadError String m) => m b
numberType = Except.throwError
  "you cannot use integer literals without a number-type declaration"

floatType :: (MonadError String m) => m b
floatType = Except.throwError
  "you cannot use float literals without a float-type declaration"

charType :: (MonadError String m) => m b
charType = Except.throwError
  "you cannot use character literals without a char-type declaration"

stringType :: (MonadError String m) => m b
stringType = Except.throwError
  "you cannot use string literals without a string-type declaration"

labelType :: (MonadError String m) => m b
labelType = Except.throwError
  "you cannot use labels without a label-type declaration"

curlyBrackets :: (MonadError String m) => m b
curlyBrackets = Except.throwError
  "you cannot use curly brackets without a curly-brackets declaration"

squareBrackets :: (MonadError String m) => m b
squareBrackets = Except.throwError
  "you cannot use square brackets without a square-brackets declaration"

annotation :: (NameError a, MonadError String m) => a -> m b
annotation n = Except.throwError $
  "type annotation for undefined function " ++ err n

noForeignAnno :: (NameError a, MonadError String m) => a -> m b
noForeignAnno n = Except.throwError $
  "no type annotation for foreign function " ++ err n

noModule :: (MonadError String m) => ModName -> m b
noModule m = Except.throwError $
  "module " ++ m ++ " does not exist"

duplicateModule :: (MonadError Msg m) => ModName -> m b
duplicateModule m = Except.throwError $ Msg $
  "Module " ++ m ++ " exists in two places"

noMainModule :: (MonadError Msg m) => m b
noMainModule = Except.throwError $ Msg
  "There is no Main module in your project"

noMainFunction :: (MonadError Msg m) => m b
noMainFunction = Except.throwError $ Msg
  "Main module does not export 'main' function"

noSrcDirectory :: (MonadError Msg m) => m b
noSrcDirectory = Except.throwError $ Msg
  "There is no \"src\" directory"

directoryRead :: (MonadError Msg m) => m b
directoryRead = Except.throwError $ Msg
  "Cannot read from directory"

mutualRecursion :: (MonadError String m) => m b
mutualRecursion = Except.throwError
  "function definitions are mutually recursive"

synonymRecursion :: (MonadError String m) => m b
synonymRecursion = Except.throwError
  "type synonym or expression synonym is recursive"

partialExpand :: (NameError a, MonadError String m) => a -> m b
partialExpand n = Except.throwError $
  "expression synonym" ++ err n ++ " is partially applied"

partialSynonym :: (NameError a, MonadError String m) => a -> m b
partialSynonym n = Except.throwError $
  "type synonym " ++ err n ++ " is partially applied"

kindInference :: (NameError a, MonadError String m) => a -> m b
kindInference n = Except.throwError $
  "kind of type variable " ++ err n ++ " cannot be inferred"

kindCheck :: (NameError a, MonadError String m) => a -> m b
kindCheck n = Except.throwError $
  "type variable " ++ err n ++ " has two different kinds"

typeVariables :: (MonadError String m) => m b
typeVariables = Except.throwError
  "too many type variables"

generalAnno :: (MonadError String m) => Name -> Type -> m b
generalAnno n t = Except.throwError $ concat
  [ "cannot bind annotated type variable ", err n, " to the following type:\n"
  , "* ", prettyType TypeOuter t, "\n"
  , "try making your type annotation more specific"
  ]

occursCheck :: (MonadError String m) => Name -> Type -> m b
occursCheck n t = Except.throwError $ concat
  [ "cannot bind type variable ", err n, " to the following type:\n"
  , "* ", prettyType TypeOuter t, "\n"
  , "doing so would produce an infinite type"
  ]

typeCall :: (MonadError String m) => Type -> m b
typeCall t = Except.throwError $ concat
  [ "cannot apply type to argument:\n"
  , "* ", prettyType TypeOuter t
  ]

unification :: (MonadError String m) => Type -> Type -> m b
unification t1 t2 = Except.throwError $ concat
  [ "cannot match types:\n"
  , "* ", prettyType TypeOuter t1, "\n"
  , "* ", prettyType TypeOuter t2
  ]

kindUnification :: (MonadError String m) => Kind -> Kind -> m b
kindUnification k1 k2 = Except.throwError $ concat
  [ "cannot match kinds:\n"
  , "* ", prettyKind KindOuter k1, "\n"
  , "* ", prettyKind KindOuter k2
  ]

noLabel :: (MonadError String m) => Type -> Type -> m b
noLabel t1 t2 = Except.throwError $ concat
  [ "cannot find label in row:\n"
  , "* ", prettyType TypeOuter t1, "\n"
  , "* ", prettyType TypeOuter t2
  ]

unsure :: (MonadError String m) => m b
unsure = Except.throwError "something went wrong"

withLocation :: (Functor m) => Location -> ExceptT String m a -> ExceptT Msg m a
withLocation (file, line, column) = Except.withExceptT
  \e -> Msg $ concat [ show file, " (line ", show line, ", column ", show column, "):\n", e ]

defContext :: (Functor m) => Identifier -> ExceptT Msg m a -> ExceptT Msg m a
defContext n = Except.withExceptT
  \(Msg e) -> Msg $ concat [ "Error in definition of ", err n, ":\n", e ]

annoContext :: (Functor m) => [Identifier] -> ExceptT Msg m a -> ExceptT Msg m a
annoContext ns = Except.withExceptT
  \(Msg e) -> Msg $ concat [ "Error in annotation for ", unwords (map err ns), ":\n", e ]

data TypeContext
  = TypeOuter
  | Operator
  | CallLeft
  | CallRight
  deriving (Eq)

prettyType :: TypeContext -> Type -> String
prettyType context tipe =
  case unforall tipe of
    ([], t) -> prettyMono context t
    (vars, t) -> parens (context /= TypeOuter) $ concat
      [ "any ", unwords vars, ". ", prettyType TypeOuter t ]

prettyMono :: TypeContext -> Type -> String
prettyMono context = \case
  TCon (Unqualified n) -> parens (operator n) n
  TCon (Qualified m n) -> m ++ "." ++ parens (operator n) n
  TVar (n, _) -> n
  TLabel n -> n

  TCall (TCall (TCon (Qualified _ n)) t1) t2 | operator n ->
    parens (context /= TypeOuter) $ concat
      [ prettyType Operator t1
      , " ", n, " "
      , prettyType Operator t2
      ]

  TCall t1 t2 ->
    parens (context == CallRight) $ concat
      [ prettyType CallLeft t1
      , " "
      , prettyType CallRight t2
      ]

  t -> prettyType context t

data KindContext
  = KindOuter
  | ArrLeft
  | ArrRight
  deriving (Eq)

prettyKind :: KindContext -> Kind -> String
prettyKind context = \case
  KType -> "Type"
  KNum -> "Num"
  KLabel -> "Label"
  KRow -> "Row"

  KArr k1 k2 ->
    parens (context == ArrLeft) $ concat
      [ prettyKind ArrLeft k1
      , " -> "
      , prettyKind ArrRight k2
      ]

parens :: Bool -> String -> String
parens cond s = if cond then "(" ++ s ++ ")" else s

operator :: String -> Bool
operator = all (`elem` opChars)
