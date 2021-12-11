{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Data.List as List
import qualified Data.Foldable as Fold
import qualified Control.Monad as Monad
import qualified Control.Exception as Ex
import qualified System.Environment as Env

import qualified Data.Map as Map

import qualified System.Directory as Dir
import qualified System.Directory.Internal.Prelude as Pre
import qualified System.Process as Proc

import qualified Compiler.Error as Error
import Compiler.Parse (parseFiles)
import Compiler.Desugar (desugarModules)
import Compiler.Unalias (unaliasModule)
import Compiler.Infer (checkModule)
import Compiler.Generate (genModule)

main :: IO ()
main = do
  prog <- Env.getProgName
  args <- Env.getArgs
  case args of
    ["init"] -> initialise
    ["compile"] -> compile False
    ["check"] -> compile True
    ["clone", user, repo] -> clone user repo
    _ -> do
      putStrLn "Valid commands:"
      putStrLn (prog ++ " init               -- create an empty Lune project in this folder")
      putStrLn (prog ++ " compile            -- convert source code into JS and HTML")
      putStrLn (prog ++ " check              -- run the type checker without compiling")
      putStrLn (prog ++ " clone [user] [foo] -- add the Github repo user/foo to dependencies")

initialise :: IO ()
initialise = do
  Dir.createDirectory "src"
  Dir.createDirectory "depends"
  writeFile "src/Main.lune" ""
  putStrLn "Lune project initialised"

compile :: Bool -> IO ()
compile checkOnly =
  tryIO safeGetFiles \(modNames, lunePaths, jsPaths) ->
  try (noDuplicates modNames) \() ->
  tryIO (parseFiles lunePaths) \modules ->
  try (desugarModules $ Map.fromList $ zip modNames modules) \defs ->
  try (unaliasModule defs) \defs' ->
  try (checkModule defs') \() ->
    if checkOnly
      then putStrLn "All is well"
      else do
        javascript <- concat <$> mapM readFile jsPaths
        writeFile "index.html" indexHtml
        writeFile "output.js" (genModule javascript defs')
        putStrLn "Compiled into \"output.js\""

clone :: String -> String -> IO ()
clone user repo = runClone
  `catch` (\_ -> putStrLn "There is no \"src\" directory")
  `Ex.finally` Dir.removePathForcibly "lune-temp"
  where
    runClone = do
      Dir.createDirectory "lune-temp"
      let url = concat [ "https://github.com/", user, "/", repo, ".git" ]
      Proc.callProcess "git" [ "clone", url, "lune-temp" ]
      moveAll "lune-temp/src" ("depends/" ++ repo)
      hasDepends <- Dir.doesDirectoryExist "lune-temp/depends"
      Monad.when hasDepends (moveAll "lune-temp/depends" "depends")
      putStrLn "Successfully cloned repo"

moveAll :: FilePath -> FilePath -> IO ()
moveAll dir dest = do
  paths <- Dir.listDirectory dir
  Dir.createDirectoryIfMissing True dest
  let move path = Dir.renamePath (dir ++ "/" ++ path) (dest ++ "/" ++ path)
  mapM_ move paths

indexHtml :: String
indexHtml = unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<body>"
  , "<script src=\"output.js\"></script>"
  , "</body>"
  , "</html>"
  ]

try :: (Show e) => Either e a -> (a -> IO ()) -> IO ()
try result f =
  case result of
    Left err -> print err
    Right x -> f x

tryIO :: (Show e) => IO (Either e a) -> (a -> IO ()) -> IO ()
tryIO action f = do
  result <- action
  try result f

type ModName = String

noDuplicates :: [ModName] -> Either Error.Msg ()
noDuplicates = Monad.void . Fold.foldrM consMaybe []
  where
    consMaybe m ms
      | m `elem` ms = Error.duplicateModule m
      | otherwise = Right (m : ms)

catch :: IO a -> (Ex.IOException -> IO a) -> IO a
catch = Ex.catch

safeGetFiles :: IO (Either Error.Msg ([ModName], [FilePath], [FilePath]))
safeGetFiles =
  fmap Right get `catch` \e ->
    if Pre.isDoesNotExistError e
      then return Error.noSrcDirectory
      else return Error.directoryRead
  where
    get = do
      src <- getFiles "src"
      packages <- Dir.listDirectory "depends"
      let qualify p = "depends/" ++ p
      depends <- mapM (catchGetFiles . qualify) packages
      return (src <> Fold.fold depends)

catchGetFiles :: FilePath -> IO ([ModName], [FilePath], [FilePath])
catchGetFiles dir = getFiles dir `catch` \_ -> return ([], [], [])

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
