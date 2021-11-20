{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Parse (parseFiles) where

import qualified Text.Read as Read
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bf
import qualified Control.Applicative as Ap
import qualified Control.Monad as Monad
import Data.Functor.Identity (Identity)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as Ex
import Text.Parsec (Parsec, (<|>))

import Syntax.Common
import Syntax.Frontend

type OpTable = Map Name (Ex.Assoc, Int)
type Parser a = Parsec String OpTable a

lexerStyle :: Token.LanguageDef OpTable
lexerStyle = Token.LanguageDef
  { Token.commentStart = "[-"
  , Token.commentEnd = "-]"
  , Token.commentLine = "--"
  , Token.nestedComments = True
  , Token.identStart = Parsec.letter
  , Token.identLetter = Parsec.alphaNum <|> Parsec.char '_'
  , Token.opStart = Parsec.oneOf opChars
  , Token.opLetter = Parsec.oneOf opChars
  , Token.reservedOpNames =
      [ "-"
      , "."
      , "="
      , "::"
      ]

  , Token.reservedNames =
      [ "do"
      , "def"
      , "in"
      , "val"
      , "let"
      , "type"
      , "where"
      , "with"
      , "foreign"
      , "syntax"
      , "infix"
      , "any"
      , "import"
      , "open"
      , "export"
      ]
  , Token.caseSensitive = True
  }

lexer :: Token.TokenParser OpTable
lexer = Token.makeTokenParser lexerStyle

Token.TokenParser
  { Token.identifier
  , Token.reserved
  , Token.operator
  , Token.reservedOp
  , Token.natural
  , Token.naturalOrFloat
  , Token.charLiteral
  , Token.stringLiteral
  , Token.commaSep1
  , Token.parens
  , Token.brackets
  , Token.braces
  , Token.whiteSpace
  } = lexer

identifierLower :: Parser String
identifierLower = do
  name <- identifier
  if Char.isUpper (head name)
    then fail []
    else return name

identifierUpper :: Parser String
identifierUpper = do
  name <- identifier
  if Char.isLower (head name)
    then fail []
    else return name

nameOrOperator :: Parser String
nameOrOperator = parens operator <|> identifierLower

parseIdentifier :: (Name -> a) -> (Identifier -> a) -> Parser a
parseIdentifier f g = do
  name <- identifier
  if Char.isUpper (head name)
    then parseQualified name <|> return (f name)
    else return $ g (Unqualified name)
  where
    parseQualified modName = do
      reservedOp "."
      name <- nameOrOperator
      return $ g (Qualified modName name)

parseNumber :: Parser Expr
parseNumber =
  naturalOrFloat >>= \case
    Left x -> return $ Int (fromInteger x)
    Right x -> return (Float x)

parseNegate :: Parser Expr
parseNegate = do
  reservedOp "-"
  fmap Negate parseFactor

parseDefIn :: Parser Expr
parseDefIn = do
  reserved "def"
  defs <- Parsec.many1 parseLocalDef
  reserved "in"
  body <- parseExpr
  return (DefIn defs body)

parseLambda :: Parser Expr
parseLambda = do
  reserved "do"
  args <- Parsec.many identifierLower
  reservedOp "."
  body <- parseExpr
  return (Lambda args body)

parseParensExpr :: Parser Expr
parseParensExpr = parens $
  parseOperator <|>
  Parsec.try parseExpr <|>
  parseLeftSection
  where
    parseOperator = do
      name <- operator
      parseRightSection name <|>
        return (Operator name Nothing Nothing)

    parseRightSection name = do
      x <- parseCallFactor
      return $ Operator name Nothing (Just x)

    parseLeftSection = do
      x <- parseCallFactor
      name <- operator
      return $ Operator name (Just x) Nothing

parseFactor :: Parser Expr
parseFactor =
  parseNumber <|>
  fmap Char charLiteral <|>
  fmap String stringLiteral <|>
  parseIdentifier Label Identifier <|>
  parseNegate <|>
  parseDefIn <|>
  parseLambda <|>
  parseParensExpr

parseCallFactor :: Parser Expr
parseCallFactor = do
  x <- parseFactor
  parseCall x <|> return x
  where
    parseCall f = do
      x <- parseFactor
      parseCall (Call f x) <|> return (Call f x)

parseExpr :: Parser Expr
parseExpr = do
  ops <- operatorParsers
    (\n x y -> Operator n (Just x) (Just y))
  Ex.buildExpressionParser ops parseCallFactor

parseTFactor :: Parser Type
parseTFactor =
  parseIdentifier TLabel TCon <|>
  fmap TRecord (braces parseType) <|>
  fmap TVariant (brackets parseType) <|>
  parens parseType

parseTCallFactor :: Parser Type
parseTCallFactor = do
  t <- parseTFactor
  parseCall t <|> return t
  where
    parseCall f = do
      t <- parseTFactor
      parseCall (TCall f t) <|> return (TCall f t)

parseType :: Parser Type
parseType = do
  ops <- operatorParsers TOperator
  Ex.buildExpressionParser ops parseTCallFactor

parseAny :: Parser Scheme
parseAny = do
  reserved "any"
  vars <- Parsec.many1 identifierLower
  reservedOp "."
  tipe <- parseType
  return (Forall vars tipe)

parseScheme :: Parser Scheme
parseScheme =
  parseAny <|>
  fmap (Forall []) parseType

parseKind :: Parser Kind
parseKind = do
  k <- parseKFactor
  parseKArr k <|> return k
  where
    parseKArr k1 = do
      reservedOp "->"
      k2 <- parseKind
      return (KArr k1 k2)

parseKFactor :: Parser Kind
parseKFactor =
  (reserved "Type" >> return KType) <|>
  (reserved "Num" >> return KNum) <|>
  (reserved "Row" >> return KRow) <|>
  (reserved "Label" >> return KLabel) <|>
  parens parseKind

parseAnnotation :: ([Name] -> Scheme -> a) -> Parser a
parseAnnotation annotation = do
  reserved "val"
  names <- commaSep1 nameOrOperator
  reservedOp "::"
  tipe <- parseScheme
  return (annotation names tipe)

parseFunc :: (Name -> [Name] -> Expr -> a) -> Parser a
parseFunc func = do
  reserved "let"
  name <- nameOrOperator
  args <- Parsec.many identifierLower
  reservedOp "="
  body <- parseExpr
  return (func name args body)

parseTypeDef :: Parser Def
parseTypeDef = do
  reserved "type"
  name <- nameOrOperator
  parseBase name <|> parseSynonym name
  where
    parseBase name = do
      reservedOp "::"
      kind <- parseKind
      wrapper <- fmap Just (parseWrapper name) <|> return Nothing
      return (Type name kind wrapper)

    parseSynonym name = do
      args <- Parsec.many identifierLower
      reservedOp "="
      body <- parseType
      return (Synonym name args body)

parseWrapper :: Name -> Parser Wrapper
parseWrapper name = do
  reserved "where"
  reserved name
  args <- Parsec.many identifierLower
  reservedOp "="
  body <- parseType
  reserved "with"
  wrapper <- identifierLower
  unwrapper <- fmap Just parseUnwrapper <|> return Nothing
  return (Wrapper args body wrapper unwrapper)
  where
    parseUnwrapper = do
      reservedOp ","
      identifierLower

parseForeign :: Parser Def
parseForeign = do
  reserved "foreign"
  name <- nameOrOperator
  args <- Parsec.many identifierLower
  reservedOp "="
  body <- stringLiteral
  return (Foreign name args body)

parseInfix :: Parser Def
parseInfix = do
  reserved "infix"
  name <- operator
  reserved "left" <|> reserved "right" <|> reserved "non"
  _ <- natural
  return (Infix name)

parseSyntax :: Parser Def
parseSyntax = do
  reserved "syntax"
  name <- nameOrOperator
  role <- parseRole
  return (Syntax name role)
  where
    when word role = reserved word >> return role
    parseRole =
      when "negate-function" NegateFunction <|>
      when "function-type" FunctionType <|>
      when "number-type" NumberType <|>
      when "float-type" FloatType <|>
      when "char-type" CharType <|>
      when "string-type" StringType <|>
      when "label-type" LabelType <|>
      when "lazy-type" LazyType <|>
      when "delay-function" DelayFunction <|>
      when "curly-brackets" CurlyBrackets <|>
      when "square-brackets" SquareBrackets <|>
      when "row-constructor" RowConstructor

parseLocalDef :: Parser LocalDef
parseLocalDef =
  parseAnnotation LAnnotation <|>
  parseFunc LFunc

parseDef :: Parser Def
parseDef =
  parseAnnotation Annotation <|>
  parseFunc Func <|>
  parseTypeDef <|>
  parseForeign <|>
  parseInfix <|>
  parseSyntax

parsePort :: Parser Port
parsePort =
  parseTypePort <|>
  parseValuePort
  where
    parseTypePort =
      reserved "type" >>
      fmap TypePort nameOrOperator

    parseValuePort = do
      fmap ValuePort nameOrOperator

parseImport :: Parser Import
parseImport = do
  reserved "import"
  modName <- identifierUpper
  parseOpen modName <|>
    parseAlias modName <|>
    parseExposing modName Nothing <|>
    return (Import modName Nothing Nothing)
  where
    parseOpen modName = do
      reserved "open"
      return (ImportOpen modName)

    parseAlias modName = do
      reserved "as"
      alias <- identifierUpper
      parseExposing modName (Just alias) <|>
        return (Import modName (Just alias) Nothing)

    parseExposing modName alias = do
      reserved "exposing"
      exposed <- commaSep1 parsePort
      return $ Import modName alias (Just exposed)

operatorParsers :: (Name -> a -> a -> a) -> Parser (Ex.OperatorTable String OpTable Identity a)
operatorParsers combine =
  fmap toList Parsec.getState
  where
    toList =
      map (map toParser)
      . List.foldl' add []
      . List.sortOn (\(n, (a, p)) -> p)
      . Map.toList

    add [] op = [[op]]
    add ops op =
      let
        (_, (_, p1)) = op
        (_, (_, p2)) = head (head ops)
      in
      if p1 > p2
        then [op] : ops
        else (op : head ops) : tail ops

    toParser (name, (assoc, _)) =
      Ex.Infix (reservedOp name >> return (combine name)) assoc

parseModule :: Parser Module
parseModule = do
  whiteSpace
  reserved "export"
  exports <- commaSep1 parsePort
  imports <- Parsec.many parseImport
  defs <- Parsec.many1 parseDef
  Parsec.eof
  return (Module exports imports defs)

data CommentState
  = Code
  | LineComment
  | BlockComment Int

removeComments :: CommentState -> String -> String
removeComments = curry $ \case
  (_, []) -> []

  (Code, '-':'-':str) -> removeComments LineComment str
  (Code, '[':'-':str) -> removeComments (BlockComment 0) str
  (Code, c:str) -> c : removeComments Code str

  (LineComment, '\n':str) -> ' ' : removeComments Code str
  (LineComment, _:str) -> removeComments LineComment str

  (BlockComment 0, '-':']':str) -> ' ' : removeComments Code str
  (BlockComment n, '-':']':str) -> removeComments (BlockComment $ n - 1) str
  (BlockComment n, '[':'-':str) -> removeComments (BlockComment $ n + 1) str
  (BlockComment n, _:str) -> removeComments (BlockComment n) str

getInfixes :: [String] -> OpTable
getInfixes = \case
  "infix" : name : assoc : prec : strs
    | all (`elem` opChars) name
    , Just assoc' <- parseAssoc assoc
    , Just prec' <- Read.readMaybe prec
    -> Map.insert name (assoc', prec') (getInfixes strs)

  [] -> Map.empty
  (_ : strs) -> getInfixes strs

  where
    parseAssoc = \case
      "left" -> Just Ex.AssocLeft
      "right" -> Just Ex.AssocRight
      "non" -> Just Ex.AssocNone
      _ -> Nothing

getOptable :: String -> OpTable
getOptable = getInfixes . words . removeComments Code

parseFiles :: [FilePath] -> IO (Either Parsec.ParseError [Module])
parseFiles paths = do
  programs <- mapM readFile paths
  let ops = getOptable (unlines programs)
  return $ Monad.zipWithM
    (Parsec.runParser parseModule ops)
    paths programs
