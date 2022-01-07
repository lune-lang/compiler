{-# LANGUAGE LambdaCase #-}

module Document.Generate (docModules) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Data.Map as Map
import Data.Map (Map)

import Syntax.Common
import Syntax.Frontend

spacesBefore :: [String] -> String
spacesBefore = concatMap (' ' :)

h1 :: String -> String
h1 text = concat [ "# ", text, "\n" ]

h4 :: String -> String
h4 text = concat [ "#### ", text, "\n" ]

codeBlock :: String -> String
codeBlock text = unlines [ "```", text, "```" ]

docName :: Name -> String
docName name
  | all (`elem` opChars) name = concat [ "(", name, ")" ]
  | otherwise = name

docDef :: ModName -> [SimplePort] -> Def -> String
docDef modName exports (def, _) = let
  exValue name = ValuePort name `elem` exports
  exType name = TypePort name `elem` exports
  in case def of
    Annotation names (_, tipe) ->
      docAnnotation modName (filter exValue names) tipe

    Foreign names (_, tipe) _ ->
      docAnnotation modName (filter exValue names) tipe

    Expand name args (_, body) | exValue name ->
      docExpand modName name args body

    Type name (_, kind) Nothing | exType name ->
      docType modName name kind

    Type name (_, kind) (Just (Wrapper args (_, tipe) maker getter)) | exType name -> let
      wrapped = unwords (name : args)
      makerType = concat [ tipe, " -> ", wrapped ]
      getterType = concat [ wrapped, " -> ", tipe ]

      maybeName (n, _)
        | exValue n = Just n
        | otherwise = Nothing

      makerName = maybeName maker
      getterName = maybeName =<< getter

      in concat
        [ docType modName name kind
        , docAnnotation modName (Maybe.maybeToList makerName) makerType
        , docAnnotation modName (Maybe.maybeToList getterName) getterType
        ]

    Synonym name args (_, body) | exType name ->
      docSynonym modName name args body

    Documentation markdown -> markdown ++ "\n"
    _ -> ""

docAnnotation :: ModName -> [Name] -> String -> String
docAnnotation modName names tipe
  | null names = ""
  | otherwise = let
    anchors = concatMap (anchorVal modName) names
    heading = h4 $ "val " ++ List.intercalate ", " (map docName names)
    code = codeBlock (":: " ++ tipe)
    in concat [ anchors, heading, code ]

docExpand :: ModName -> Name -> [Name] -> String -> String
docExpand modName name args body = let
  anchor = anchorVal modName name
  heading = h4 ("expand " ++ docName name)
  code = codeBlock $ concat [ docName name, spacesBefore args, " = ", body ]
  in concat [ anchor, heading, code ]

docType :: ModName -> Name -> String -> String
docType modName name kind = let
  anchor = anchorType modName name
  heading = h4 ("type " ++ docName name)
  code = codeBlock (":: " ++ kind)
  in concat [ anchor, heading, code ]

docSynonym :: ModName -> Name -> [Name] -> String -> String
docSynonym modName name args body = let
  anchor = anchorType modName name
  heading = h4 ("type " ++ docName name)
  code = codeBlock $ concat [ docName name, spacesBefore args, " = ", body ]
  in concat [ anchor, heading, code ]

anchorVal :: ModName -> Name -> String
anchorVal modName name = concat
  [ "<a name=\"", modName, ".", docName name, "\"></a>\n"]

anchorType :: ModName -> Name -> String
anchorType modName = anchorVal ("type_" ++ modName)

docModule :: (ModName, Module) -> String
docModule (modName, m) = let
  doc = docDef modName $ map fst (getExports m)
  in h1 modName ++ concatMap doc (getDefs m)

docModules :: Map ModName Module -> String
docModules = concatMap docModule . Map.toList
