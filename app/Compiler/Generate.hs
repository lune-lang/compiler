{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Compiler.Generate (genModule) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Function((&))

import qualified Data.Map as Map
import Data.Map ((!))

import Syntax.Desugared
import Syntax.Common

makeValid :: Name -> Name
makeValid name
  | all (`elem` opChars) name = map (opSubs !) name ++ "$"
  | name `elem` reserved = "$" ++ name
  | otherwise = name
  where
    opSubs = Map.fromList (zip opChars ['a'..])
    reserved =
      [ "Array", "Date", "Infinity"
      , "Math", "NaN", "Number"
      , "Object", "String", "abstract"
      , "alert", "all", "anchor"
      , "anchors", "area", "arguments"
      , "assign", "await", "blur"
      , "boolean", "break", "button"
      , "byte", "case", "catch"
      , "char", "checkbox", "class"
      , "clearInterval", "clearTimeout", "clientInformation"
      , "close", "closed", "confirm"
      , "const", "constructor", "continue"
      , "crypto", "debugger", "decodeURI"
      , "decodeURIComponent", "default", "defaultStatus"
      , "delete", "do", "document"
      , "double", "element", "elements"
      , "else", "embed", "embeds"
      , "encodeURI", "encodeURIComponent", "enum"
      , "escape", "eval", "event"
      , "export", "extends", "false"
      , "fileUpload", "final", "finally"
      , "float", "focus", "for"
      , "form", "forms", "frame"
      , "frameRate", "frames", "function"
      , "goto", "hasOwnProperty", "hidden"
      , "history", "if", "image"
      , "images", "implements", "import"
      , "in", "innerHeight", "innerWidth"
      , "instanceof", "int", "interface"
      , "isFinite", "isNaN", "isPrototypeOf"
      , "layer", "layers", "length"
      , "let", "link", "location"
      , "long", "mimeTypes", "name"
      , "native", "navigate", "navigator"
      , "new", "null", "offscreenBuffering"
      , "onblur", "onclick", "onerror"
      , "onfocus", "onkeydown", "onkeypress"
      , "onkeyup", "onload", "onmousedown"
      , "onmouseover", "onmouseup", "onsubmit"
      , "open", "opener", "option"
      , "outerHeight", "outerWidth", "package"
      , "packages", "pageXOffset", "pageYOffset"
      , "parent", "parseFloat", "parseInt"
      , "password", "pkcs11", "plugin"
      , "private", "prompt", "propertyIsEnum"
      , "protected", "prototype", "public"
      , "radio", "reset", "return"
      , "screenX", "screenY", "scroll"
      , "secure", "select", "self"
      , "setInterval", "setTimeout", "short"
      , "static", "status", "submit"
      , "super", "switch", "synchronized"
      , "taint", "text", "textarea"
      , "this", "throw", "throws"
      , "toString", "top", "transient"
      , "true", "try", "typeof"
      , "undefined", "unescape", "untaint"
      , "valueOf", "var", "void"
      , "volatile", "while", "window"
      , "with", "yield"
      ]

genExpr :: Expr -> String
genExpr (expr, _) =
  case expr of
    Int x -> show x
    Float x -> show x
    Char x -> show x
    String x -> show x
    Label x -> show x
    Identifier n -> genIdentifier n
    DefIn n _ x y -> genDefIn n x y
    Lambda n x -> genLambda n x
    Call f x -> genCall f x

genIdentifier :: Identifier -> String
genIdentifier = \case
  Unqualified n -> makeValid n
  Qualified m n -> concat
    [ makeValid m, "[", show n, "]" ]

genDefIn :: Name -> Expr -> Expr -> String
genDefIn n x y = concat
  [ "(function() { var "
  , makeValid n, " = ", genExpr x
  , "; return ", genExpr y, "; })()"
  ]

genLambda :: Name -> Expr -> String
genLambda n x = concat
  [ "function(", makeValid n
  , ") { return ", genExpr x, "; }"
  ]

genCall :: Expr -> Expr -> String
genCall f x = concat
  [ genExpr f, "(", genExpr x, ")" ]

modNameIdentifier :: Identifier -> Maybe Name
modNameIdentifier = \case
  Unqualified _ -> Nothing
  Qualified m _ -> Just m

modNames :: Module -> [Name]
modNames m =
  foreigns ++ funcs
  & Maybe.mapMaybe modNameIdentifier
  & List.nub
  where
    foreigns = Map.keys (getForeigns m)
    funcs = map (\(n, _, _, _) -> n) (getFuncs m)

genModuleDef :: ModName -> String
genModuleDef m = concat
  [ "var ", makeValid m, " = {}; " ]

genFunc :: Identifier -> Expr -> String
genFunc n x = concat
  [ genIdentifier n, " = ", genExpr x, ";\n" ]

genWrapper :: Wrapper -> String
genWrapper (Wrapper _ _ mk gt) =
  genIdentity mk ++ foldMap genIdentity gt
  where
    genIdentity n =
      genIdentifier n
      ++ " = function(x) { return x; };\n"

genModule :: String -> Module -> String
genModule js m = concat
  [ concatMap genModuleDef (modNames m)
  , "\n", js
  , wrapperDefs
  , funcDefs
  , genIdentifier (Qualified "Main" "main"), "();"
  ]
  where
    funcDefs =
      getFuncs m
      & map (\(n, _, x, _) -> (n, x))
      & concatMap (uncurry genFunc)

    wrapperDefs =
      getTypes m
      & Map.elems
      & Maybe.mapMaybe snd
      & concatMap genWrapper
