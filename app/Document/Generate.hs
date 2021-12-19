module Document.Generate (docModules) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Data.Map as Map
import Data.Map (Map)

import Syntax.Common
import Syntax.Frontend

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

    Foreign names (_, tipe) ->
      docAnnotation modName (filter exValue names) tipe

    Expand name args (_, body) | exValue name ->
      docExpand modName name args body

    Type name (_, kind) Nothing | exType name ->
      docType modName name kind

    Type name (_, kind) (Just (Wrapper args (_, tipe) maker getter)) | exType name -> let
      wrapped = concat [ name, " ", unwords args ]
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

    Documentation markdown -> indent markdown
    _ -> ""

docAnnotation :: ModName -> [Name] -> String -> String
docAnnotation modName names tipe
  | null names = ""
  | otherwise = let
    anchors = unlines $ map (anchorLink modName) names
    heading = codeHeading $ concat
      [ List.intercalate ", " (map docName names), " :: ", tipe ]
    in anchors ++ heading

docExpand :: ModName -> Name -> [Name] -> String -> String
docExpand modName name args body = let
  anchor = anchorLink modName name
  heading = codeHeading $ concat
    [ "expand ", docName name, " ", unwords args, " = ", body ]
  in anchor ++ heading

docType :: ModName -> Name -> String -> String
docType modName name kind = let
  anchor = anchorLink modName name
  heading = codeHeading $ concat
    [ "type ", docName name, " :: ", kind ]
  in anchor ++ heading

docSynonym :: ModName -> Name -> [Name] -> String -> String
docSynonym modName name args body = let
  anchor = anchorLink modName name
  heading = codeHeading $ concat
    [ "type ", docName name, " ", unwords args, " = ", body ]
  in anchor ++ heading

indent :: String -> String
indent = unlines . map ("  " ++) . lines

anchorLink :: ModName -> Name -> String
anchorLink modName name = concat
  [ "<a name=\"", modName, ".", docName name, "\">"]

codeHeading :: String -> String
codeHeading str = let
  heading = unlines [ "<h4>\n", indent str, "</h4>" ]
  in indent heading

docModule :: (ModName, Module) -> String
docModule (modName, m) = unlines
  [ "# " ++ modName
  , "<details>"
  , "  <summary></summary>\n"
  , concatMap (docDef modName $ map fst $ getExports m) (getDefs m)
  , "</details>\n"
  ]

docModules :: Map ModName Module -> String
docModules = concatMap docModule . Map.toList
