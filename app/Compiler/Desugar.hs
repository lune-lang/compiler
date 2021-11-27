{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

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

import qualified Control.Lens as Lens

data Interface = Interface
  { _values :: [(Name, Location)]
  , _types :: [(Name, Location)]
  }

Lens.makeLenses ''Interface

emptyInterface :: Interface
emptyInterface = Interface [] []

getValueNames :: Interface -> [Name]
getValueNames = map fst . Lens.view values

getTypeNames :: Interface -> [Name]
getTypeNames = map fst . Lens.view types

data VarMap = VarMap
  { _valueSubs :: Map Identifier Identifier
  , _typeSubs :: Map Identifier Identifier
  }

Lens.makeLenses ''VarMap

emptyVars :: VarMap
emptyVars = VarMap Map.empty Map.empty

type Desugar = ExceptT Error.Msg (ReaderT (Map Role Identifier, VarMap) (State Int))

makeId :: Desugar Int
makeId = do
  x <- State.get
  State.modify (+1)
  return x

type Insert a b = a -> b -> Desugar a

insertValue :: Insert Interface (Name, Location)
insertValue interface (name, loc) =
  if name `elem` getValueNames interface
    then Error.withLocation loc (Error.multipleDef name)
    else return $ Lens.over values ((name, loc):) interface

insertType :: Insert Interface (Name, Location)
insertType interface (name, loc) =
  if name `elem` getTypeNames interface
    then Error.withLocation loc (Error.multipleDef name)
    else return $ Lens.over types ((name, loc):) interface

insertDef :: [F.SimplePort] -> Insert Interface F.Def
insertDef exports interface (def, loc) =
  case def of
    F.Annotation _ _ -> return interface
    F.Func name _ _ -> valueDef interface (name, loc)
    F.Foreign name _ _ -> valueDef interface (name, loc)
    F.Type name _ Nothing -> typeDef interface (name, loc)
    F.Type name _ (Just (F.Wrapper _ _ maker getter)) -> do
      interface' <- typeDef interface (name, loc)
      interface'' <- valueDef interface' maker
      Monad.foldM valueDef interface'' getter
    F.Synonym name _ _ -> typeDef interface (name, loc)
    F.Infix _ -> return interface
    F.Syntax _ _ -> return interface
  where
    valueDef interface' (name, loc) =
      if F.ValuePort name `elem` exports
        then insertValue interface' (name, loc)
        else return interface'

    typeDef interface' (name, loc) =
      if F.TypePort name `elem` exports
        then insertType interface' (name, loc)
        else return interface'

addExport :: [F.SimplePort] -> [F.SimplePort] -> F.Port -> Desugar [F.SimplePort]
addExport available exports (e, loc)
  | e `elem` exports = Error.withLocation loc (Error.multipleExport e)
  | e `elem` available = return (e : exports)
  | otherwise = Error.withLocation loc (Error.exportFailure e)

getExports :: F.Module -> Desugar [F.SimplePort]
getExports m =
  Monad.foldM (addExport available) [] (F.getExports m)
  where
    available = concatMap toPort (F.getDefs m)
    toPort (def, _) =
      case def of
        F.Annotation _ _ -> []
        F.Func name _ _ -> [F.ValuePort name]
        F.Foreign name _ _ -> [F.ValuePort name]
        F.Type name _ Nothing -> [F.TypePort name]
        F.Type name _ (Just (F.Wrapper _ _ maker getter)) ->
          [ F.TypePort name, F.ValuePort (fst maker) ] ++
          map (F.ValuePort . fst) (Maybe.maybeToList getter)
        F.Synonym name _ _ -> [F.TypePort name]
        F.Infix _ -> []
        F.Syntax _ _ -> []

getInterface :: ModName -> F.Module -> Desugar Interface
getInterface modName m = do
  exports <- getExports m
  Monad.foldM (insertDef exports) emptyInterface (F.getDefs m)

insertValueSub :: Location -> Insert VarMap (Identifier, Identifier)
insertValueSub loc vars (name, sub) =
  if Map.member name (Lens.view valueSubs vars)
    then Error.withLocation loc (Error.multipleDef name)
    else return $ Lens.over valueSubs (Map.insert name sub) vars

insertTypeSub :: Location -> Insert VarMap (Identifier, Identifier)
insertTypeSub loc vars (name, sub) =
  if Map.member name (Lens.view typeSubs vars)
    then Error.withLocation loc (Error.multipleDef name)
    else return $ Lens.over typeSubs (Map.insert name sub) vars

insertLocalValue :: Insert VarMap (Name, Location)
insertLocalValue vars (name, loc) =
  insertValueSub loc vars (Unqualified name, Unqualified name)

insertImportValue :: (Name -> Identifier) -> ModName -> Insert VarMap (Name, Location)
insertImportValue f modName vars (name, loc) =
  insertValueSub loc vars (f name, Qualified modName name)

insertImportType :: (Name -> Identifier) -> ModName -> Insert VarMap (Name, Location)
insertImportType f modName vars (name, loc) =
  insertTypeSub loc vars (f name, Qualified modName name)

insertInterface :: (Name -> Identifier) -> ModName -> Insert VarMap Interface
insertInterface f modName vars (Interface values types) = do
  vars' <- Monad.foldM (insertImportValue f modName) vars values
  Monad.foldM (insertImportType f modName) vars' types

insertExposed :: ModName -> Interface -> Insert VarMap F.Port
insertExposed modName interface vars (port, loc) =
  case port of
    F.ValuePort name | name `elem` getValueNames interface ->
      insertImportValue Unqualified modName vars (name, loc)
    F.TypePort name | name `elem` getTypeNames interface ->
      insertImportType Unqualified modName vars (name, loc)
    name -> Error.withLocation loc (Error.importFailure name)

insertImport :: Map ModName Interface -> Insert VarMap F.Import
insertImport interfaces vars (imp, loc) =
  case imp of
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
          Nothing -> Error.withLocation loc (Error.noModule modName)
          Just interface -> do
            vars' <- insertInterface f modName vars interface
            Monad.foldM (insertExposed modName interface) vars' exposed

insertTopLevelDef :: ModName -> Insert VarMap F.Def
insertTopLevelDef modName vars (def, loc) =
  case def of
    F.Annotation _ _ -> return vars
    F.Func name _ _ -> addValue vars (name, loc)
    F.Foreign name _ _ -> addValue vars (name, loc)
    F.Type name _ Nothing -> addType vars (name, loc)
    F.Type name _ (Just (F.Wrapper _ _ maker getter)) -> do
      vars' <- addType vars (name, loc)
      vars'' <- addValue vars' maker
      Monad.foldM addValue vars'' getter
    F.Synonym name _ _ -> addType vars (name, loc)
    F.Infix _ -> return vars
    F.Syntax _ _ -> return vars
    where
      addType = insertImportType Unqualified modName
      addValue = insertImportValue Unqualified modName

insertLocalDef :: Insert VarMap F.LocalDef
insertLocalDef vars (def, loc) =
  case def of
    F.LAnnotation _ _ -> return vars
    F.LFunc name _ _ -> insertLocalValue vars (name, loc)

makeJavascript :: [Name] -> String -> String
makeJavascript args body =
  foldr makeJS body args
  where
    makeJS arg body = concat
      [ "function(", arg
      , "){ return ", body
      , "; }"
      ]

desugarDefs :: ModName -> [F.Def] -> Desugar Module
desugarDefs modName defs = do
  (annos, funcs, foreigns, types, synonyms) <-
    Monad.foldM addDef (Map.empty, [], Map.empty, Map.empty, []) defs
  let funcNames = map (\(n, _, _) -> n) funcs
  let foreignNames = map fst (Map.keys foreigns)
  mapM_ (existsAnno $ funcNames ++ foreignNames) (Map.keys annos)
  let funcs' = mergeAnnos annos funcs
  foreigns' <- mergeForeignAnnos annos foreigns
  syntax <- Reader.asks fst
  return (Module funcs' foreigns' types synonyms syntax)
  where
    addDef (annos, funcs, foreigns, types, synonyms) (def, loc) =
      case def of
        F.Annotation names tipe -> do
          let names' = map (Qualified modName) names
          tipe' <- Error.annoContext names' (desugarScheme tipe)
          let keys = zip names' (repeat loc)
          let newAnnos = Map.fromList (zip keys $ repeat tipe') <> annos
          return (newAnnos, funcs, foreigns, types, synonyms)

        F.Func name args body@(_, bodyLoc) -> do
          let name' = Qualified modName name
          body' <- Error.defContext name' $
            desugarExpr (F.Lambda args body, bodyLoc)
          let newFuncs = (name', body', loc) : funcs
          return (annos, newFuncs, foreigns, types, synonyms)

        F.Foreign name args js -> do
          let name' = Qualified modName name
          let js' = makeJavascript args js
          let newForeigns = Map.insert (name', loc) js' foreigns
          return (annos, funcs, newForeigns, types, synonyms)

        F.Type name kind Nothing -> do
          let name' = Qualified modName name
          let newTypes = Map.insert name' (kind, Nothing) types
          return (annos, funcs, foreigns, newTypes, synonyms)

        F.Type name kind (Just (F.Wrapper args tipe maker getter)) -> do
          let name' = Qualified modName name
          tipe' <- Error.defContext name' (desugarType args tipe)
          let maker' = Qualified modName (fst maker)
          let getter' = fmap (Qualified modName . fst) getter
          let newTypes = Map.insert name' (kind, Just (Wrapper args tipe' maker' getter')) types
          return (annos, funcs, foreigns, newTypes, synonyms)

        F.Synonym name args tipe -> do
          let name' = Qualified modName name
          tipe' <- Error.defContext name' (desugarType args tipe)
          let newSynonyms = (name', args, tipe', loc) : synonyms
          return (annos, funcs, foreigns, types, newSynonyms)

        F.Infix _ -> return (annos, funcs, foreigns, types, synonyms)

        F.Syntax name role -> do
          (VarMap valueNames typeNames) <- Reader.asks snd
          let functionRoles = [ NegateFunction, DelayFunction ]
          let available = if role `elem` functionRoles then valueNames else typeNames
          if Qualified modName name `notElem` available
            then Error.withLocation loc (Error.notDefined name)
            else return (annos, funcs, foreigns, types, synonyms)

desugarLocalDefs :: [F.LocalDef] -> Desugar [(Name, Maybe Scheme, Expr, Location)]
desugarLocalDefs defs = do
  (annos, funcs) <- Monad.foldM addDef (Map.empty, []) defs
  let funcNames = map (\(n, _, _) -> n) funcs
  mapM_ (existsAnno funcNames) (Map.keys annos)
  arrange (mergeAnnos annos funcs)
  where
    addDef defs (def, loc) =
      case def of
        F.LAnnotation names tipe -> do
          tipe' <- desugarScheme tipe
          let keys = zip names (repeat loc)
          let annos = Map.fromList $ zip keys (repeat tipe')
          return $ Bf.first (annos <>) defs

        F.LFunc name args body@(_, loc) -> do
          body' <- desugarExpr (F.Lambda args body, loc)
          return $ Bf.second ((name, body', loc) :) defs

existsAnno :: (Eq a, Error.NameError a) => [a] -> (a, Location) -> Desugar ()
existsAnno funcs (name, loc)
  | name `elem` funcs = return ()
  | otherwise = Error.withLocation loc (Error.annotation name)

mergeAnnos :: Ord a => Map (a, Location) Scheme -> [(a, Expr, Location)] -> [(a, Maybe Scheme, Expr, Location)]
mergeAnnos annos = map \(name, body, loc) ->
  (name, Map.lookup name (Map.mapKeys fst annos), body, loc)

mergeForeignAnnos :: Map (Identifier, a) Scheme -> Map (Identifier, Location) String -> Desugar (Map Identifier (Scheme, String))
mergeForeignAnnos annos =
  sequence .
  Map.mapKeys fst .
  Map.mapWithKey \(name, loc) js ->
    case Map.lookup name (Map.mapKeys fst annos) of
      Nothing -> Error.withLocation loc (Error.noForeignAnno name)
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

arrange :: Variable a => [(a, Maybe Scheme, Expr, Location)] -> Desugar [(a, Maybe Scheme, Expr, Location)]
arrange funcs = do
  order <- getOrder referenceMap
  let position name = Maybe.fromMaybe 0 (List.elemIndex name order)
  return $ List.sortOn (position . nameOf) funcs
  where
    nameOf (n, _, _, _) = n
    vars = Set.filter (`elem` map nameOf funcs) . freeVars
    referenceMap = Map.fromList $ map (\(n, _, x, loc) -> (n, (vars x, loc))) funcs

getOrder :: Variable a => Map a (Set a, Location) -> Desugar [a]
getOrder refs
  | Map.null refs = return []
  | Set.null roots = Error.withLocation loc Error.mutualRecursion
  | otherwise = (Set.toList roots ++) <$> getOrder rest
  where
    loc = snd $ snd $ head (Map.toList refs)
    isRoot n (rs, _) = Set.null rs || Set.toList rs == [n]
    roots = Map.keysSet (Map.filterWithKey isRoot refs)
    rest = Bf.first (\\ roots) <$> foldr Map.delete refs roots

freeCons :: Type -> Set Identifier
freeCons = \case
  TCon n -> Set.singleton n
  TVar _ -> Set.empty
  TLabel _ -> Set.empty
  TCall func arg -> freeCons func <> freeCons arg

arrangeSynonyms :: [(Identifier, [Name], Type, Location)] -> Desugar [(Identifier, [Name], Type, Location)]
arrangeSynonyms syns = do
  order <- getSynonymOrder referenceMap
  let position name = Maybe.fromMaybe 0 (List.elemIndex name order)
  return $ List.sortOn (position . nameOf) syns
  where
    nameOf (n, _, _, _) = n
    cons = Set.filter (`elem` map nameOf syns) . freeCons
    removeArgs = map (\(n, _, t, loc) -> (n, (t, loc)))
    referenceMap =
      map (Bf.second $ Bf.first cons) (removeArgs syns)
      & Map.fromList

getSynonymOrder :: Map Identifier (Set Identifier, Location) -> Desugar [Identifier]
getSynonymOrder refs
  | Map.null refs = return []
  | Set.null roots = Error.withLocation loc Error.synonymRecursion
  | otherwise = (Set.toList roots ++) <$> getOrder rest
  where
    loc = snd $ snd $ head (Map.toList refs)
    isRoot n (rs, _) = Set.null rs
    roots = Map.keysSet (Map.filterWithKey isRoot refs)
    rest = Bf.first (\\ roots) <$> foldr Map.delete refs roots

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
    variablesDef (def, _) =
       case def of
         F.LAnnotation _ _ -> Set.empty
         F.LFunc _ _ x -> variableNames x

desugarExpr :: F.Expr -> Desugar Expr
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
        Nothing -> Error.withLocation loc Error.negateFunction
        Just name | name `elem` Lens.view valueSubs vars -> do
          id1 <- makeId
          withId $ Call (Identifier name, loc, id1) value'
        Just name -> Error.withLocation loc (Error.notDefined name)

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
        let defIn (n, a, b, _) x = withId (DefIn n a b x)
        Fold.foldrM defIn body' defs'

    F.Lambda args body -> do
      local <- Monad.foldM insertLocalValue emptyVars $ zip args (repeat loc)
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
        vars <- Reader.asks snd :: Desugar VarMap
        case Map.lookup name (Lens.view valueSubs vars) of
          Nothing -> Error.withLocation loc (Error.notDefined name)
          Just sub -> withId $ f (Identifier sub)

desugarScheme :: F.Scheme -> Desugar Scheme
desugarScheme (F.Forall as tipe) =
  Forall as <$> desugarType as tipe

desugarType :: [Name] -> F.Type -> Desugar Type
desugarType as (tipe, loc) =
  case tipe of
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
        Nothing -> Error.withLocation loc Error.curlyBrackets
        Just name | name `elem` Lens.view typeSubs vars -> do
          return $ TCall (TCon name) row'
        Just name -> Error.withLocation loc (Error.notDefined name)

    F.TVariant row -> do
      (syntax, vars) <- Reader.ask
      row' <- desugarType as row
      case Map.lookup SquareBrackets syntax of
        Nothing -> Error.withLocation loc Error.squareBrackets
        Just name | name `elem` Lens.view typeSubs vars -> do
          return $ TCall (TCon name) row'
        Just name -> Error.withLocation loc (Error.notDefined name)

    F.TCall func arg -> TCall
      <$> desugarType as func
      <*> desugarType as arg

  where
    ifDefined name f = do
      vars <- Reader.ask
      case Map.lookup name (Lens.view typeSubs vars) of
        Nothing -> Error.withLocation loc (Error.notDefined name)
        Just sub -> return $ f (TCon sub)

desugarModule :: Map ModName Interface -> ModName -> F.Module -> Desugar Module
desugarModule interfaces modName m = do
  exports <- getExports m
  vars <- Monad.foldM (insertImport interfaces) emptyVars (F.getImports m)
  vars' <- Monad.foldM (insertTopLevelDef modName) vars (F.getDefs m)
  Reader.local (Bf.second $ const vars') $
    desugarDefs modName (F.getDefs m)

desugarModules :: Map ModName F.Module -> Either Error.Msg Module
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
      funcs' <- arrange funcs
      synonyms' <- arrangeSynonyms synonyms
      return (Module funcs' foreigns types synonyms' syntax)

hasMain :: Map ModName Interface -> Desugar ()
hasMain interfaces =
  case Map.lookup "Main" interfaces of
    Nothing -> Error.noMainModule
    Just (funcs, _) | "main" `elem` funcs -> return ()
    Just _ -> Error.noMainFunction

noRepeatedInfix :: [F.Def] -> Desugar ()
noRepeatedInfix =
  Monad.foldM_ addInfix []
  where
    addInfix ops = \case
      F.Infix name
        | name `elem` ops -> Error.multipleInfix name
        | otherwise -> return (name : ops)
      _ -> return ops :: Desugar [Name]

getSyntaxMap :: Map ModName [F.Def] -> Desugar (Map Role Identifier)
getSyntaxMap modules = do
  (roles, syntax) <- Monad.foldM addSyntax ([], Map.empty) defs
  return syntax
  where
    pair m ds = zip (repeat m) ds
    defs = Fold.fold (Map.mapWithKey pair modules)

    addSyntax (roles, syntax) = \case
      (modName, F.Syntax name role) ->
        let name' = Qualified modName name in
        if role `elem` roles
          then Error.conflictingSyntax name'
          else return (role : roles, Map.insert role name' syntax)
      _ -> return (roles, syntax)
