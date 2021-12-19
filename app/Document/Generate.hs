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

docDef :: [SimplePort] -> Def -> String
docDef exports (def, _) = let
  exValue name = ValuePort name `elem` exports
  exType name = TypePort name `elem` exports
  in case def of
    Annotation names (_, tipe) ->
      docAnnotation (filter exValue names) tipe

    Foreign names (_, tipe) ->
      docAnnotation (filter exValue names) tipe

    Expand name args (_, body) | exValue name ->
      docExpand name args body

    Type name (_, kind) Nothing | exType name ->
      docType name kind

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
        [ docType name kind
        , docAnnotation (Maybe.maybeToList makerName) makerType
        , docAnnotation (Maybe.maybeToList getterName) getterType
        ]

    Synonym name args (_, body) | exType name ->
      docSynonym name args body

    Documentation markdown -> indent markdown
    _ -> ""

docAnnotation :: [Name] -> String -> String
docAnnotation names tipe
  | null names = ""
  | otherwise = codeHeading $ concat
    [ List.intercalate ", " (map docName names), " :: ", tipe ]

docExpand :: Name -> [Name] -> String -> String
docExpand name args body =
  codeHeading $ concat
    [ "expand ", docName name, " ", unwords args, " = ", body ]

docType :: Name -> String -> String
docType name kind =
  codeHeading $ concat
    [ "type ", docName name, " :: ", kind ]

docSynonym :: Name -> [Name] -> String -> String
docSynonym name args body =
  codeHeading $ concat
    [ "type ", docName name, " ", unwords args, " = ", body ]

indent :: String -> String
indent = unlines . map ("  " ++) . lines

codeHeading :: String -> String
codeHeading str = let
  heading = unlines [ "<h4>\n", indent str, "</h4>" ]
  in indent heading

docModule :: (ModName, Module) -> String
docModule (modName, m) = unlines
  [ "# " ++ modName
  , "<details>"
  , "  <summary></summary>\n"
  , concatMap (docDef $ map fst $ getExports m) (getDefs m)
  , "</details>\n"
  ]

docModules :: Map ModName Module -> String
docModules = concatMap docModule . Map.toList
