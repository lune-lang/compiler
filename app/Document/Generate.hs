{-# LANGUAGE LambdaCase #-}

module Document.Generate (docModules) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified System.FilePath as File
import System.FilePath ((</>))

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

docDef :: [SimplePort] -> Def -> String
docDef exports (def, _) = let
  exValue name = ValuePort name `elem` exports
  exType name = TypePort name `elem` exports
  in case def of
    Annotation names (_, tipe) ->
      docAnnotation (filter exValue names) tipe

    Foreign names (_, tipe) _ ->
      docAnnotation (filter exValue names) tipe

    Expand name args (_, body) | exValue name ->
      docExpand name args body

    Type name (_, kind) Nothing | exType name ->
      docType name kind

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
        [ docType name kind
        , docAnnotation (Maybe.maybeToList makerName) makerType
        , docAnnotation (Maybe.maybeToList getterName) getterType
        ]

    Synonym name args (_, body) | exType name ->
      docSynonym name args body

    Documentation markdown -> markdown ++ "\n"
    _ -> ""

docAnnotation :: [Name] -> String -> String
docAnnotation names tipe
  | null names = ""
  | otherwise = let
    anchors = concatMap anchorVal names
    heading = h4 $ "val " ++ List.intercalate ", " (map docName names)
    code = codeBlock (":: " ++ tipe)
    in concat [ anchors, heading, code ]

docExpand :: Name -> [Name] -> String -> String
docExpand name args body = let
  anchor = anchorVal name
  heading = h4 ("expand " ++ docName name)
  code = codeBlock $ concat [ docName name, spacesBefore args, " = ", body ]
  in concat [ anchor, heading, code ]

docType :: Name -> String -> String
docType name kind = let
  anchor = anchorType name
  heading = h4 ("type " ++ docName name)
  code = codeBlock (":: " ++ kind)
  in concat [ anchor, heading, code ]

docSynonym :: Name -> [Name] -> String -> String
docSynonym name args body = let
  anchor = anchorType name
  heading = h4 ("type " ++ docName name)
  code = codeBlock $ concat [ docName name, spacesBefore args, " = ", body ]
  in concat [ anchor, heading, code ]

anchorLink :: String -> String
anchorLink name = concat
  [ "<a name=\"", name, "\"></a>\n" ]

anchorVal :: Name -> String
anchorVal = anchorLink . docName

anchorType :: Name -> String
anchorType = anchorLink . ("type-" ++) . docName

replaceFirst :: String -> FilePath -> FilePath
replaceFirst dir path =
  case File.splitPath path of
    [] -> ""
    _:rest -> dir </> concat rest

docModule :: (ModName, FilePath, Module) -> (FilePath, String)
docModule (modName, path, m) = let
  path' = replaceFirst "doc" (File.replaceExtension path "md")
  heading = h1 modName
  doc = docDef $ map fst (getExports m)
  defs = concatMap doc (getDefs m)
  in (path', heading ++ defs)

docModules :: [(ModName, FilePath, Module)] -> [(FilePath, String)]
docModules = map docModule
