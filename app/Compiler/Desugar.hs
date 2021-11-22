{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Desugar (desugarModules) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Bifunctor as Bf
import qualified Data.Foldable as Fold
import qualified Control.Monad as Monad
import Data.Function ((&))
import Data.Functor ((<&>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set, (\\))

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)

import qualified Compiler.Error as Error
import qualified Syntax.Frontend as F
import Syntax.Desugared
import Syntax.Common

type Desugar e = ExceptT e (ReaderT (Map Role Identifier, VarMap) (State Int))

makeId :: Desugar e Int
makeId = do
  x <- State.get
  State.modify (+1)
  return x

type Insert a b = a -> b -> Desugar String a

type Interface = ([Name], [Name])

emptyInterface :: Interface
emptyInterface = ([], [])

insertValue :: Insert Interface Name
insertValue interface name =
  if name `elem` fst interface
    then Error.multipleDef name
    else return $ Bf.first (name:) interface

insertType :: Insert Interface Name
insertType interface name =
  if name `elem` snd interface
    then Error.multipleDef name
    else return $ Bf.second (name:) interface

insertDef :: [F.Port] -> Insert Interface F.Def
insertDef exports interface = \case
  F.Annotation _ _ -> return interface
  F.Func name _ _ -> valueDef interface name
  F.Foreign name _ _ -> valueDef interface name
  F.Type name _ Nothing -> typeDef interface name
  F.Type name _ (Just (F.Wrapper _ _ maker getter)) -> do
    interface' <- typeDef interface name
    interface'' <- valueDef interface' maker
    Monad.foldM valueDef interface'' getter
  F.Synonym name _ _ -> typeDef interface name
  F.Infix _ -> return interface
  F.Syntax _ _ -> return interface
  where
    valueDef interface' name =
      if F.ValuePort name `elem` exports
        then insertValue interface' name
        else return interface'

    typeDef interface' name =
      if F.TypePort name `elem` exports
        then insertType interface' name
        else return interface'

addExport :: [F.Port] -> [F.Port] -> F.Port -> Desugar String [F.Port]
addExport available exports e
  | e `elem` exports = Error.multipleExport e
  | e `elem` available = return (e : exports)
  | otherwise = Error.exportFailure e

getExports :: F.Module -> Desugar String [F.Port]
getExports m =
  Monad.foldM (addExport available) [] (F.getExports m)
  where
    available = concatMap toPort (F.getDefs m)
    toPort = \case
      F.Annotation _ _ -> []
      F.Func name _ _ -> [F.ValuePort name]
      F.Foreign name _ _ -> [F.ValuePort name]
      F.Type name _ Nothing -> [F.TypePort name]
      F.Type name _ (Just (F.Wrapper _ _ maker getter)) ->
        [ F.TypePort name, F.ValuePort maker ] ++
        map F.ValuePort (Maybe.maybeToList getter)
      F.Synonym name _ _ -> [F.TypePort name]
      F.Infix _ -> []
      F.Syntax _ _ -> []

getInterface :: ModName -> F.Module -> Desugar Error.Context Interface
getInterface modName m =
  Error.moduleContext modName do
    exports <- getExports m
    Monad.foldM (insertDef exports) emptyInterface (F.getDefs m)

type VarMap = (Map Identifier Identifier, Map Identifier Identifier)

emptyVars :: VarMap
emptyVars = (Map.empty, Map.empty)

insertValueSub :: Insert VarMap (Identifier, Identifier)
insertValueSub vars (name, sub) =
  if Map.member name (fst vars)
    then Error.multipleDef name
    else return $ Bf.first (Map.insert name sub) vars

insertTypeSub :: Insert VarMap (Identifier, Identifier)
insertTypeSub vars (name, sub) =
  if Map.member name (snd vars)
    then Error.multipleDef name
    else return $ Bf.second (Map.insert name sub) vars

insertLocalValue :: Insert VarMap Name
insertLocalValue vars name =
  insertValueSub vars (Unqualified name, Unqualified name)

insertImportValue :: (Name -> Identifier) -> ModName -> Insert VarMap Name
insertImportValue f modName vars name =
  insertValueSub vars (f name, Qualified modName name)

insertImportType :: (Name -> Identifier) -> ModName -> Insert VarMap Name
insertImportType f modName vars name =
  insertTypeSub vars (f name, Qualified modName name)

insertInterface :: (Name -> Identifier) -> ModName -> Insert VarMap Interface
insertInterface f modName vars (values, types) = do
  vars' <- Monad.foldM (insertImportValue f modName) vars values
  Monad.foldM (insertImportType f modName) vars' types

insertExposed :: ModName -> Interface -> Insert VarMap F.Port
insertExposed modName (values, types) vars = \case
  F.ValuePort name | name `elem` values ->
    insertImportValue Unqualified modName vars name
  F.TypePort name | name `elem` types ->
    insertImportType Unqualified modName vars name
  name -> Error.importFailure name

insertImport :: Map ModName Interface -> Insert VarMap F.Import
insertImport interfaces vars = \case
  F.ImportOpen modName -> insertModule Unqualified modName []
  F.Import modName maybeAlias maybeExposed ->
    let
      alias = Maybe.fromMaybe modName maybeAlias
      exposed = Maybe.fromMaybe [] maybeExposed
    in
    insertModule (Qualified alias) modName exposed
  where
    insertModule f modName exposed =
      case Map.lookup modName interfaces of
        Nothing -> Error.noModule modName
        Just interface -> do
          vars' <- insertInterface f modName vars interface
          Monad.foldM (insertExposed modName interface) vars' exposed

insertTopLevelDef :: ModName -> Insert VarMap F.Def
insertTopLevelDef modName vars = \case
  F.Annotation _ _ -> return vars
  F.Func name _ _ -> addValue vars name
  F.Foreign name _ _ -> addValue vars name
  F.Type name _ Nothing -> addType vars name
  F.Type name _ (Just (F.Wrapper _ _ maker getter)) -> do
    vars' <- addType vars name
    vars'' <- addValue vars' maker
    Monad.foldM addValue vars'' getter
  F.Synonym name _ _ -> addType vars name
  F.Infix _ -> return vars
  F.Syntax _ _ -> return vars
  where
    addType = insertImportType Unqualified modName
    addValue = insertImportValue Unqualified modName

insertLocalDef :: Insert VarMap F.LocalDef
insertLocalDef vars = \case
  F.LAnnotation _ _ -> return vars
  F.LFunc name _ _ -> insertLocalValue vars name

makeJavascript :: [Name] -> String -> String
makeJavascript args body =
  foldr makeJS body args
  where
    makeJS arg body = concat
      [ "function(", arg
      , "){ return ", body
      , "; }"
      ]

desugarDefs :: ModName -> [F.Def] -> Desugar Error.Context Module
desugarDefs modName defs = do
  (annos, funcs, foreigns, types, synonyms) <-
    Monad.foldM addDef (Map.empty, [], Map.empty, Map.empty, []) defs
  Error.moduleContext modName $ mapM_ (existsAnno $ map fst funcs ++ Map.keys foreigns) (Map.keys annos)
  let funcs' = mergeAnnos annos funcs
  foreigns' <- Error.moduleContext modName (mergeForeignAnnos annos foreigns)
  syntax <- Reader.asks fst
  return (Module funcs' foreigns' types synonyms syntax)
  where
    addDef (annos, funcs, foreigns, types, synonyms) = \case
      F.Annotation names tipe -> do
        let names' = map (Qualified modName) names
        tipe' <- Error.annoContext names' (desugarScheme tipe)
        let newAnnos = Map.fromList (zip names' $ repeat tipe') <> annos
        return (newAnnos, funcs, foreigns, types, synonyms)

      F.Func name args body@(_, loc) -> do
        let name' = Qualified modName name
        body' <- Error.defContext name' $
          desugarExpr (F.Lambda args body, loc)
        let newFuncs = (name', body') : funcs
        return (annos, newFuncs, foreigns, types, synonyms)

      F.Foreign name args js -> do
        let name' = Qualified modName name
        let js' = makeJavascript args js
        let newForeigns = Map.insert name' js' foreigns
        return (annos, funcs, newForeigns, types, synonyms)

      F.Type name kind Nothing -> do
        let name' = Qualified modName name
        let newTypes = Map.insert name' (kind, Nothing) types
        return (annos, funcs, foreigns, newTypes, synonyms)

      F.Type name kind (Just (F.Wrapper args tipe maker getter)) -> do
        let name' = Qualified modName name
        tipe' <- Error.defContext name' (desugarType args tipe)
        let maker' = Qualified modName maker
        let getter' = fmap (Qualified modName) getter
        let newTypes = Map.insert name' (kind, Just (Wrapper args tipe' maker' getter')) types
        return (annos, funcs, foreigns, newTypes, synonyms)

      F.Synonym name args tipe -> do
        let name' = Qualified modName name
        tipe' <- Error.defContext name' (desugarType args tipe)
        let newSynonyms = (name', args, tipe') : synonyms
        return (annos, funcs, foreigns, types, newSynonyms)

      F.Infix _ -> return (annos, funcs, foreigns, types, synonyms)

      F.Syntax name role -> do
        vars <- Reader.asks snd
        let functionRoles = [ NegateFunction, DelayFunction ]
        let available = if role `elem` functionRoles then fst vars else snd vars
        Error.moduleContext modName
          if Qualified modName name `notElem` available
            then Error.notDefined name
            else return (annos, funcs, foreigns, types, synonyms)

desugarLocalDefs :: [F.LocalDef] -> Desugar String [(Name, Maybe Scheme, Expr)]
desugarLocalDefs defs = do
  (annos, funcs) <- Monad.foldM addDef (Map.empty, []) defs
  mapM_ (existsAnno $ map fst funcs) (Map.keys annos)
  arrange (mergeAnnos annos funcs)
  where
    addDef defs' = \case
      F.LAnnotation names tipe -> do
        tipe' <- desugarScheme tipe
        let annos = Map.fromList $ zip names (repeat tipe')
        return $ Bf.first (annos <>) defs'

      F.LFunc name args body@(_, loc) -> do
        body' <- desugarExpr (F.Lambda args body, loc)
        return $ Bf.second ((name, body') :) defs'

existsAnno :: (Eq a, Error.NameError a) => [a] -> a -> Desugar String ()
existsAnno funcs name
  | name `elem` funcs = return ()
  | otherwise = Error.annotation name

mergeAnnos :: Ord a => Map a Scheme -> [(a, Expr)] -> [(a, Maybe Scheme, Expr)]
mergeAnnos annos = map \(name, body) ->
  (name, Map.lookup name annos, body)

mergeForeignAnnos :: Map Identifier Scheme -> Map Identifier String -> Desugar String (Map Identifier (Scheme, String))
mergeForeignAnnos annos = sequence .
  Map.mapWithKey \name js ->
    case Map.lookup name annos of
      Nothing -> Error.noForeignAnno name
      Just tipe -> return (tipe, js)

class Ord a => Variable a where
  fromIdentifier :: Identifier -> Set a
  unqualified :: Name -> a

instance Variable Name where
  fromIdentifier = \case
    Unqualified n -> Set.singleton n
    Qualified _ _ -> Set.empty
  unqualified = id

instance Variable Identifier where
  fromIdentifier = Set.singleton
  unqualified = Unqualified

freeVars :: Variable a => Expr -> Set a
freeVars (expr, _, _) =
  case expr of
    Int _ -> Set.empty
    Float _ -> Set.empty
    Char _ -> Set.empty
    String _ -> Set.empty
    Label _ -> Set.empty

    Identifier n -> fromIdentifier n

    DefIn name _ value body -> Set.delete (unqualified name)
      (freeVars value <> freeVars body)

    Lambda name body -> Set.delete (unqualified name)
      (freeVars body)

    Call func arg -> freeVars func <> freeVars arg

arrange :: Variable a => [(a, Maybe Scheme, Expr)] -> Desugar String [(a, Maybe Scheme, Expr)]
arrange funcs = do
  order <- getOrder referenceMap
  let position name = Maybe.fromMaybe 0 (List.elemIndex name order)
  return $ List.sortOn (position . nameOf) funcs
  where
    nameOf (n, _, _) = n
    vars = Set.filter (`elem` map nameOf funcs) . freeVars
    referenceMap = Map.fromList $ map (\(n, _, x) -> (n, vars x)) funcs

getOrder :: Variable a => Map a (Set a) -> Desugar String [a]
getOrder refs
  | Map.null refs = return []
  | Set.null roots = Error.mutualRecursion
  | otherwise = (Set.toList roots ++) <$> getOrder rest
  where
    isRoot n rs = Set.null rs || Set.toList rs == [n]
    roots = Map.keysSet (Map.filterWithKey isRoot refs)
    rest = (\\ roots) <$> foldr Map.delete refs roots

freeCons :: Type -> Set Identifier
freeCons = \case
  TCon n -> Set.singleton n
  TVar _ -> Set.empty
  TLabel _ -> Set.empty
  TCall func arg -> freeCons func <> freeCons arg

arrangeSynonyms :: [(Identifier, [Name], Type)] -> Desugar String [(Identifier, [Name], Type)]
arrangeSynonyms syns = do
  order <- getSynonymOrder referenceMap
  let position name = Maybe.fromMaybe 0 (List.elemIndex name order)
  return $ List.sortOn (position . nameOf) syns
  where
    nameOf (n, _, _) = n
    cons = Set.filter (`elem` map nameOf syns) . freeCons
    removeArgs = map (\(n, _, t) -> (n, t))
    referenceMap =
      map (Bf.second cons) (removeArgs syns)
      & Map.fromList

getSynonymOrder :: Map Identifier (Set Identifier) -> Desugar String [Identifier]
getSynonymOrder refs
  | Map.null refs = return []
  | Set.null roots = Error.synonymRecursion
  | otherwise = (Set.toList roots ++) <$> getOrder rest
  where
    isRoot n rs = Set.null rs
    roots = Map.keysSet (Map.filterWithKey isRoot refs)
    rest = (\\ roots) <$> foldr Map.delete refs roots

variableNames :: F.Expr -> Set Name
variableNames (expr, _) =
  case expr of
    F.Int _ -> Set.empty
    F.Float _ -> Set.empty
    F.Char _ -> Set.empty
    F.String _ -> Set.empty
    F.Label _ -> Set.empty

    F.Identifier (Unqualified n) -> Set.singleton n
    F.Identifier _ -> Set.empty
    F.Operator _ _ _ -> Set.empty

    F.DefIn ds x -> foldMap variablesDef ds <> variableNames x
    F.Lambda _ x -> variableNames x
    F.Call x y -> variableNames x <> variableNames y
  where
    variablesDef = \case
       F.LAnnotation _ _ -> Set.empty
       F.LFunc _ _ x -> variableNames x

desugarExpr :: F.Expr -> Desugar String Expr
desugarExpr (expr, loc) =
  case expr of
    F.Int x -> withId (Int x)
    F.Float x -> withId (Float x)
    F.Char x -> withId (Char x)
    F.String x -> withId (String x)
    F.Label x -> withId (Label x)

    F.Identifier name -> ifDefined name id

    F.Negate value -> do
      (syntax, vars) <- Reader.ask
      value' <- desugarExpr value
      case Map.lookup NegateFunction syntax of
        Nothing -> Error.negateFunction
        Just name | name `elem` fst vars -> do
          id1 <- makeId
          withId $ Call (Identifier name, loc, id1) value'
        Just name -> Error.notDefined name

    F.Operator name Nothing Nothing ->
      ifDefined (Unqualified name) id

    F.Operator name (Just x) Nothing -> do
      x' <- desugarExpr x
      id1 <- makeId
      ifDefined (Unqualified name)
        \sub -> Call (sub, loc, id1) x'

    F.Operator name Nothing (Just x) -> do
      x' <- desugarExpr x
      id1 <- makeId
      id2 <- makeId
      id3 <- makeId
      id4 <- makeId
      ifDefined (Unqualified name)
        \sub -> Lambda safeVar
          (Call (Call (sub, loc, id1) (varRef, loc, id2), loc, id3)
            x', loc, id4)

    F.Operator name (Just x) (Just y) -> do
      x' <- desugarExpr x
      y' <- desugarExpr y
      id1 <- makeId
      id2 <- makeId
      ifDefined (Unqualified name)
        \sub -> Call (Call (sub, loc, id1) x', loc, id2) y'

    F.DefIn defs body -> do
      local <- Monad.foldM insertLocalDef emptyVars defs
      Reader.local (Bf.second (local <>)) do
        defs' <- desugarLocalDefs defs
        body' <- desugarExpr body
        let defIn (n, a, b) x = withId (DefIn n a b x)
        Fold.foldrM defIn body' defs'

    F.Lambda args body -> do
      local <- Monad.foldM insertLocalValue emptyVars args
      Reader.local (Bf.second (local <>)) do
        body' <- desugarExpr body
        let lambda n x = withId (Lambda n x)
        Fold.foldrM lambda body' args

    F.Call func arg -> do
      func' <- desugarExpr func
      arg' <- desugarExpr arg
      withId (Call func' arg')

    where
      withId x = (x, loc,) <$> makeId

      safeVar =
        [1..] >>= flip Monad.replicateM ['a'..'z']
        & filter (\n -> Set.notMember n $ variableNames (expr, loc))
        & head

      varRef = Identifier (Unqualified safeVar)

      ifDefined name f = do
        vars <- Reader.asks snd :: Desugar String VarMap
        case Map.lookup name (fst vars) of
          Nothing -> Error.notDefined name
          Just sub -> withId $ f (Identifier sub)

desugarScheme :: F.Scheme -> Desugar String Scheme
desugarScheme (F.Forall as tipe) =
  Forall as <$> desugarType as tipe

desugarType :: [Name] -> F.Type -> Desugar String Type
desugarType as = \case
  F.TCon (Unqualified name) | name `elem` as ->
    return $ TVar (name, Annotated)

  F.TCon name -> ifDefined name id

  F.TOperator name x y -> do
    x' <- desugarType as x
    y' <- desugarType as y
    ifDefined (Unqualified name)
      \sub -> TCall (TCall sub x') y'

  F.TLabel x -> return (TLabel x)

  F.TRecord row -> do
    (syntax, vars) <- Reader.ask
    row' <- desugarType as row
    case Map.lookup CurlyBrackets syntax of
      Nothing -> Error.curlyBrackets
      Just name | name `elem` snd vars -> do
        return $ TCall (TCon name) row'
      Just name -> Error.notDefined name

  F.TVariant row -> do
    (syntax, vars) <- Reader.ask
    row' <- desugarType as row
    case Map.lookup SquareBrackets syntax of
      Nothing -> Error.squareBrackets
      Just name | name `elem` snd vars -> do
        return $ TCall (TCon name) row'
      Just name -> Error.notDefined name

  F.TCall func arg -> TCall
    <$> desugarType as func
    <*> desugarType as arg

  where
    ifDefined name f = do
      vars <- Reader.asks snd :: Desugar String VarMap
      case Map.lookup name (snd vars) of
        Nothing -> Error.notDefined name
        Just sub -> return $ f (TCon sub)

desugarModule :: Map ModName Interface -> ModName -> F.Module -> Desugar Error.Context Module
desugarModule interfaces modName m = do
  exports <- Error.moduleContext modName (getExports m)
  vars <- Error.moduleContext modName $
    Monad.foldM (insertImport interfaces) emptyVars (F.getImports m)
  vars' <- Error.moduleContext modName $
    Monad.foldM (insertTopLevelDef modName) vars (F.getDefs m)
  Reader.local (Bf.second $ const vars') $
    desugarDefs modName (F.getDefs m)

desugarModules :: Map ModName F.Module -> Either Error.Context Module
desugarModules modules =
  State.evalState (Reader.runReaderT (Except.runExceptT desugar) (Map.empty, emptyVars)) 0
  where
    desugar = do
      interfaces <- sequence (Map.mapWithKey getInterface modules)
      hasMain interfaces
      noRepeatedInfix (concatMap F.getDefs modules)
      syntax <- getSyntaxMap (fmap F.getDefs modules)
      modules' <- Reader.local (Bf.first $ const syntax) $
        sequence $ Map.mapWithKey (desugarModule interfaces) modules
      let Module funcs foreigns types synonyms syntax = Fold.fold modules'
      funcs' <- Error.noContext (arrange funcs)
      synonyms' <- Error.noContext (arrangeSynonyms synonyms)
      return (Module funcs' foreigns types synonyms' syntax)

hasMain :: Map ModName Interface -> Desugar Error.Context ()
hasMain interfaces =
  Error.noContext
  case Map.lookup "Main" interfaces of
    Nothing -> Error.noModule "Main"
    Just (funcs, _) | "main" `elem` funcs -> return ()
    Just _ -> Error.noMain

noRepeatedInfix :: [F.Def] -> Desugar Error.Context ()
noRepeatedInfix =
  Error.noContext . Monad.foldM_ addInfix []
  where
    addInfix ops = \case
      F.Infix name
        | name `elem` ops -> Error.multipleInfix name
        | otherwise -> return (name : ops)
      _ -> return ops :: Desugar String [Name]

getSyntaxMap :: Map ModName [F.Def] -> Desugar Error.Context (Map Role Identifier)
getSyntaxMap modules = do
  (roles, syntax) <- Monad.foldM addSyntax ([], Map.empty) defs
  return syntax
  where
    pair m ds = zip (repeat m) ds
    defs = Fold.fold (Map.mapWithKey pair modules)

    addSyntax (roles, syntax) = \case
      (modName, F.Syntax name role) ->
        let name' = Qualified modName name in
        Error.moduleContext modName
        if role `elem` roles
          then Error.conflictingSyntax name'
          else return (role : roles, Map.insert role name' syntax)
      _ -> return (roles, syntax)
