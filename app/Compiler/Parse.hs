{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Compiler.Parse (parseFiles) where

import qualified Text.Read as Read
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Control.Monad as Monad
import Data.Functor.Identity (Identity)

import qualified Data.Map as Map

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as Ex
import Text.Parsec (Parsec, (<|>))

import Syntax.Common
import Syntax.Frontend

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
      , "expand"
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

identifier, operator :: Parser String
reserved, reservedOp :: String -> Parser ()
symbol :: String -> Parser String
natural :: Parser Integer
naturalOrFloat :: Parser (Either Integer Double)
charLiteral :: Parser Char
stringLiteral :: Parser String
commaSep1 :: Parser a -> Parser [a]
parens, brackets :: Parser a -> Parser a
whiteSpace :: Parser ()

Token.TokenParser
  { Token.identifier
  , Token.operator
  , Token.reserved
  , Token.reservedOp
  , Token.symbol
  , Token.natural
  , Token.naturalOrFloat
  , Token.charLiteral
  , Token.stringLiteral
  , Token.commaSep1
  , Token.parens
  , Token.brackets
  , Token.whiteSpace
  } = lexer

braces :: Parser a -> Parser a
braces parse = do
  Parsec.notFollowedBy (symbol "{-")
  Parsec.between (symbol "{") (symbol "}") parse

withText :: Parser a -> Parser (WithText a)
withText parse = do
  input <- Parsec.getInput
  result <- parse
  input' <- Parsec.getInput
  let len = length input - length input'
  let text = take len input
  return (result, removeComments Code text)

getLocation :: Parser Location
getLocation = do
  position <- Parsec.getPosition
  let file = Parsec.sourceName position
  let line = Parsec.sourceLine position
  let column = Parsec.sourceColumn position
  return (file, line, column)

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

parseIdentifier :: (Name -> a) -> (Identifier -> a) -> Parser (a, Location)
parseIdentifier f g = do
  loc <- getLocation
  name <- identifier
  if Char.isUpper (head name)
    then parseQualified loc name <|> return (f name, loc)
    else return (g $ Unqualified name, loc)
  where
    parseQualified loc modName = do
      reservedOp "."
      name <- nameOrOperator
      return (g $ Qualified modName name, loc)

parseNumber :: Parser Expr
parseNumber = do
  loc <- getLocation
  naturalOrFloat >>= \case
    Left x -> return (Int $ fromInteger x, loc)
    Right x -> return (Float x, loc)

parseChar :: Parser Expr
parseChar = do
  loc <- getLocation
  x <- charLiteral
  return (Char x, loc)

parseString :: Parser Expr
parseString = do
  loc <- getLocation
  x <- stringLiteral
  return (String x, loc)

parseNegate :: Parser Expr
parseNegate = do
  loc <- getLocation
  reservedOp "-"
  x <- parseFactor
  return (Negate x, loc)

parseDefIn :: Parser Expr
parseDefIn = do
  loc <- getLocation
  reserved "def"
  defs <- Parsec.many1 parseLocalDef
  reserved "in"
  body <- parseExpr
  return (DefIn defs body, loc)

parseLambda :: Parser Expr
parseLambda = do
  loc <- getLocation
  reserved "do"
  args <- Parsec.many identifierLower
  reservedOp "."
  body <- parseExpr
  return (Lambda args body, loc)

parseDelay :: Parser Expr
parseDelay = do
  loc <- getLocation
  expr <- braces parseExpr
  return (Delay expr, loc)

parseParensExpr :: Parser Expr
parseParensExpr = parens $
  parseOperator <|>
  Parsec.try parseExpr <|>
  parseLeftSection
  where
    parseOperator = do
      loc <- getLocation
      name <- operator
      parseRightSection name <|>
        return (Operator name Nothing Nothing, loc)

    parseRightSection name = do
      loc <- getLocation
      x <- parseCallFactor
      return (Operator name Nothing (Just x), loc)

    parseLeftSection = do
      loc <- getLocation
      x <- parseCallFactor
      name <- operator
      return (Operator name (Just x) Nothing, loc)

parseFactor :: Parser Expr
parseFactor =
  parseNumber <|>
  parseChar <|>
  parseString <|>
  parseIdentifier Label Identifier <|>
  parseNegate <|>
  parseDefIn <|>
  parseLambda <|>
  parseDelay <|>
  parseParensExpr

parseCallFactor :: Parser Expr
parseCallFactor = do
  x <- parseFactor
  parseCall x <|> return x
  where
    parseCall f = do
      loc <- getLocation
      x <- parseFactor
      let call = (Call f x, loc)
      parseCall call <|> return call

parseExpr :: Parser Expr
parseExpr = do
  loc <- getLocation
  ops <- operatorParsers
    \n x y -> (Operator n (Just x) (Just y), loc)
  Ex.buildExpressionParser ops parseCallFactor

parseTRecord :: Parser Type
parseTRecord = do
  loc <- getLocation
  row <- braces parseType
  return (TRecord row, loc)

parseTVariant :: Parser Type
parseTVariant = do
  loc <- getLocation
  row <- brackets parseType
  return (TVariant row, loc)

parseTFactor :: Parser Type
parseTFactor =
  parseIdentifier TLabel TCon <|>
  parseTRecord <|>
  parseTVariant <|>
  parens parseType

parseTCallFactor :: Parser Type
parseTCallFactor = do
  t <- parseTFactor
  parseCall t <|> return t
  where
    parseCall f = do
      loc <- getLocation
      t <- parseTFactor
      let call = (TCall f t, loc)
      parseCall call <|> return call

parseType :: Parser Type
parseType = do
  loc <- getLocation
  ops <- operatorParsers \n x y -> (TOperator n x y, loc)
  Ex.buildExpressionParser ops parseTCallFactor

parseAny :: Parser Scheme
parseAny = do
  reserved "any"
  vars <- Parsec.many1 identifierLower
  reservedOp "."
  Forall vars <$> parseType

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
      _ <- symbol "->"
      KArr k1 <$> parseKind

parseKFactor :: Parser Kind
parseKFactor =
  (reserved "Type" >> return KType) <|>
  (reserved "Num" >> return KNum) <|>
  (reserved "Row" >> return KRow) <|>
  (reserved "Label" >> return KLabel) <|>
  parens parseKind

parseAnnotation :: String -> Parser a -> ([Name] -> a -> b) -> Parser b
parseAnnotation word parse annotation = do
  reserved word
  names <- commaSep1 nameOrOperator
  reservedOp "::"
  annotation names <$> parse

parseFunc :: String -> Parser a -> (Name -> [Name] -> a -> b) -> Parser b
parseFunc word parse func = do
  reserved word
  name <- nameOrOperator
  args <- Parsec.many identifierLower
  reservedOp "="
  func name args <$> parse

parseTypeDef :: Parser SimpleDef
parseTypeDef = do
  reserved "type"
  name <- nameOrOperator
  parseBase name <|> parseSynonym name
  where
    parseBase name = do
      reservedOp "::"
      kind <- withText parseKind
      wrapper <- fmap Just (parseWrapper name) <|> return Nothing
      return (Type name kind wrapper)

    parseSynonym name = do
      args <- Parsec.many identifierLower
      reservedOp "="
      Synonym name args <$> withText parseType

parseWrapper :: Name -> Parser Wrapper
parseWrapper name = do
  reserved "where"
  reserved name
  args <- Parsec.many identifierLower
  reservedOp "="
  body <- withText parseType
  reserved "with"
  loc <- getLocation
  wrapper <- identifierLower
  unwrapper <- fmap Just parseUnwrapper <|> return Nothing
  return $ Wrapper args body (wrapper, loc) unwrapper
  where
    parseUnwrapper = do
      loc <- getLocation
      _ <- symbol ","
      unwrapper <- identifierLower
      return (unwrapper, loc)

parseInfix :: Parser SimpleDef
parseInfix = do
  reserved "infix"
  name <- operator
  assoc <-
    (reserved "left" >> return LeftAssoc) <|>
    (reserved "right" >> return RightAssoc) <|>
    (reserved "non" >> return NonAssoc)
  prec <- fromInteger <$> natural
  return (Infix name assoc prec)

parseSyntax :: Parser SimpleDef
parseSyntax = do
  reserved "syntax"
  Syntax <$> nameOrOperator <*> parseRole
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
      when "curly-brackets" CurlyBrackets <|>
      when "square-brackets" SquareBrackets <|>
      when "row-constructor" RowConstructor

parseDocumentation :: Parser SimpleDef
parseDocumentation = do
  _ <- symbol "{-"
  Documentation <$> Parsec.manyTill Parsec.anyChar (symbol "-}")

parseLocalDef :: Parser LocalDef
parseLocalDef = do
  loc <- getLocation
  def <-
    parseAnnotation "val" parseScheme LAnnotation <|>
    parseFunc "let" parseExpr LFunc
  return (def, loc)

parseDef :: Parser Def
parseDef = do
  loc <- getLocation
  def <-
    parseAnnotation "val" (withText parseScheme) Annotation <|>
    parseAnnotation "foreign" (withText parseScheme) Foreign <|>
    parseFunc "let" parseExpr Func <|>
    parseFunc "expand" (withText parseExpr) Expand <|>
    parseTypeDef <|>
    parseInfix <|>
    parseSyntax <|>
    parseDocumentation
  return (def, loc)

parsePort :: Parser Port
parsePort = do
  loc <- getLocation
  port <-
    parseTypePort <|>
    parseValuePort
  return (port, loc)
  where
    parseTypePort =
      reserved "type" >>
      fmap TypePort nameOrOperator

    parseValuePort = do
      fmap ValuePort nameOrOperator

parseImport :: Parser Import
parseImport = do
  loc <- getLocation
  reserved "import"
  modName <- identifierUpper
  settings <-
    parseOpen modName <|>
    parseAlias modName <|>
    parseExposing modName Nothing <|>
    return (Import modName Nothing Nothing)
  return (settings, loc)
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
      . List.sortOn (\(_, (_, p)) -> p)
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

    convert = \case
      LeftAssoc -> Ex.AssocLeft
      RightAssoc -> Ex.AssocRight
      NonAssoc -> Ex.AssocNone

    toParser (name, (assoc, _)) =
      Ex.Infix (reservedOp name >> return (combine name)) (convert assoc)

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
  | DocComment
  | BlockComment Int

removeComments :: CommentState -> String -> String
removeComments = curry \case
  (_, []) -> []

  (Code, '-':'-':str) -> removeComments LineComment str
  (Code, '{':'-':str) -> removeComments DocComment str
  (Code, '[':'-':str) -> removeComments (BlockComment 0) str
  (Code, c:str) -> c : removeComments Code str

  (LineComment, '\n':str) -> ' ' : removeComments Code str
  (LineComment, _:str) -> removeComments LineComment str

  (DocComment, '-':'}':str) -> ' ' : removeComments Code str
  (DocComment, _:str) -> removeComments DocComment str

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
      "left" -> Just LeftAssoc
      "right" -> Just RightAssoc
      "non" -> Just NonAssoc
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
