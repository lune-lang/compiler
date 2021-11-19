{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.List as List
import qualified Data.Foldable as Fold
import qualified Control.Exception as Ex
import qualified System.Environment as Env

import qualified Data.Map as Map

import qualified System.Directory as Dir
import qualified System.Directory.Internal.Prelude as Pre

import Compiler.Parse (parseFiles)
import Compiler.Desugar (desugarModules)
import Compiler.Infer (checkModule)
import Compiler.Generate (genModule)

main :: IO ()
main = do
  prog <- Env.getProgName
  args <- Env.getArgs
  case args of
    ["init"] -> initialise
    ["compile"] -> compile
    _ -> do
      putStrLn "Valid commands:"
      putStrLn (prog ++ " init")
      putStrLn (prog ++ " compile")

initialise :: IO ()
initialise = do
  Dir.createDirectory "src"
  writeFile "src/Main.lune" ""
  writeFile "index.html" indexHtml

indexHtml :: String
indexHtml = unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<body>"
  , "<script src=\"project.js\"></script>"
  , "</body>"
  , "</html>"
  ]

compile :: IO ()
compile =
  safeGetFiles >>= \case
    Left err -> putStrLn err
    Right (modNames, lunePaths, jsPaths) -> do
      putStrLn "Parsing files..."
      parseFiles lunePaths >>= \case
        Left err -> print err
        Right modules -> do
          putStrLn "Desugaring code..."
          case desugarModules $ Map.fromList (zip modNames modules) of
            Left err -> print err
            Right defs -> do
              putStrLn "Checking types..."
              case checkModule defs of
                Left err -> print err
                Right () -> do
                  putStrLn "Generating javascript..."
                  javascript <- mapM readFile jsPaths
                  putStrLn "Compiled into \"project.js\""
                  writeFile "project.js" $ genModule (concat javascript) defs

type ModName = String

safeGetFiles :: IO (Either String ([ModName], [FilePath], [FilePath]))
safeGetFiles =
  fmap Right (getFiles "src") `Ex.catch` \e ->
    if Pre.isDoesNotExistError e
      then return (Left "Error: there is no 'src' directory")
      else return (Left "Error: failed to read from directory")

getFiles :: FilePath -> IO ([ModName], [FilePath], [FilePath])
getFiles dir = do
  paths <- Dir.listDirectory dir
  let subDirs = filter ('.' `notElem`) paths
  let lunePaths = filter (".lune" `List.isSuffixOf`) paths
  let jsPaths = filter (".js" `List.isSuffixOf`) paths
  let modNames = map (dropEnd 5) lunePaths
  (innerModNames, innerLunePaths, innerJsPaths) <-
    Fold.fold <$> mapM getFiles' subDirs
  let lunePaths' = map qualify lunePaths ++ innerLunePaths
  let jsPaths' = map qualify jsPaths ++ innerJsPaths
  let modNames' = modNames ++ innerModNames
  return (modNames', lunePaths', jsPaths')
  where
    dropEnd x = reverse . drop x . reverse
    qualify p = dir ++ "/" ++ p
    getFiles' d = do
      (modNames, lunePaths, jsPaths) <- getFiles (qualify d)
      return (map ((d ++ "_") ++) modNames, lunePaths, jsPaths)
