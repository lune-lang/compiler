{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Desugar (desugarModules) where

import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bf
import qualified Data.Foldable as Fold
import qualified Control.Monad as Monad

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader)

import qualified Compiler.Arrange as Arrange
import qualified Compiler.Error as Error
import qualified Syntax.Frontend as F
import Syntax.Desugared
import Syntax.Common

import qualified Control.Lens as Lens
import Control.Lens ((^.))

zipRepeat :: [a] -> b -> [(a, b)]
zipRepeat xs y = zip xs (repeat y)

data Interface = Interface
  { _interfaceValues :: [(Name, Location)]
  , _interfaceTypes :: [(Name, Location)]
  }

Lens.makeLenses ''Interface

emptyInterface :: Interface
emptyInterface = Interface [] []

getValueNames :: Interface -> [Name]
getValueNames = map fst . Lens.view interfaceValues

getTypeNames :: Interface -> [Name]
getTypeNames = map fst . Lens.view interfaceTypes

data VarMap = VarMap
  { _valueSubs :: Map Identifier Identifier
  , _typeSubs :: Map Identifier Identifier
  }

Lens.makeLenses ''VarMap

instance Semigroup VarMap where
  VarMap v1 t1 <> VarMap v2 t2 =
    VarMap (v1 <> v2) (t1 <> t2)

emptyVars :: VarMap
emptyVars = VarMap Map.empty Map.empty

type SyntaxMap = Map Role (Identifier, Location)
type Desugar = ExceptT Error.Msg (Reader (SyntaxMap, VarMap))

type Insert a b = a -> b -> Desugar a

insertValue :: Insert Interface (Name, Location)
insertValue interface (name, loc) =
  if name `elem` getValueNames interface
    then Error.withLocation loc (Error.multipleDef name)
    else return $ Lens.over interfaceValues ((name, loc):) interface

insertType :: Insert Interface (Name, Location)
insertType interface (name, loc) =
  if name `elem` getTypeNames interface
    then Error.withLocation loc (Error.multipleDef name)
    else return $ Lens.over interfaceTypes ((name, loc):) interface

insertDef :: [F.SimplePort] -> Insert Interface F.Def
insertDef exports interface (def, loc) =
  case def of
    F.Annotation _ _ -> return interface
    F.Foreign names _ _ -> Monad.foldM valueDef interface (zipRepeat names loc)
    F.Func name _ _ -> valueDef interface (name, loc)
    F.Expand name _ _ -> valueDef interface (name, loc)
    F.Type name _ Nothing -> typeDef interface (name, loc)
    F.Type name _ (Just (F.Wrapper _ _ maker getter)) -> do
      interface' <- typeDef interface (name, loc)
      interface'' <- valueDef interface' maker
      Monad.foldM valueDef interface'' getter
    F.Synonym name _ _ -> typeDef interface (name, loc)
    F.Infix {} -> return interface
    F.Syntax _ _ -> return interface
    F.Documentation _ -> return interface
  where
    valueDef interface' (name, loc') =
      if F.ValuePort name `elem` exports
        then insertValue interface' (name, loc')
        else return interface'

    typeDef interface' (name, loc') =
      if F.TypePort name `elem` exports
        then insertType interface' (name, loc')
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
        F.Foreign names _ _ -> map F.ValuePort names
        F.Func name _ _ -> [F.ValuePort name]
        F.Expand name _ _ -> [F.ValuePort name]
        F.Type name _ Nothing -> [F.TypePort name]
        F.Type name _ (Just (F.Wrapper _ _ maker getter)) ->
          [ F.TypePort name, F.ValuePort (fst maker) ] ++
          map (F.ValuePort . fst) (Maybe.maybeToList getter)
        F.Synonym name _ _ -> [F.TypePort name]
        F.Infix {} -> []
        F.Syntax _ _ -> []
        F.Documentation _ -> []

getInterface :: F.Module -> Desugar Interface
getInterface m = do
  exports <- getExports m
  Monad.foldM (insertDef exports) emptyInterface (F.getDefs m)

insertValueSub :: Location -> Insert VarMap (Identifier, Identifier)
insertValueSub loc vars (name, sub) =
  if Map.member name (vars ^. valueSubs)
    then Error.withLocation loc (Error.multipleDef name)
    else return $ Lens.over valueSubs (Map.insert name sub) vars

insertTypeSub :: Location -> Insert VarMap (Identifier, Identifier)
insertTypeSub loc vars (name, sub) =
  if Map.member name (vars ^. typeSubs)
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
    F.Foreign names _ _ -> Monad.foldM addValue vars (zipRepeat names loc)
    F.Func name _ _ -> addValue vars (name, loc)
    F.Expand name _ _ -> addValue vars (name, loc)
    F.Type name _ Nothing -> addType vars (name, loc)
    F.Type name _ (Just (F.Wrapper _ _ maker getter)) -> do
      vars' <- addType vars (name, loc)
      vars'' <- addValue vars' maker
      Monad.foldM addValue vars'' getter
    F.Synonym name _ _ -> addType vars (name, loc)
    F.Infix {} -> return vars
    F.Syntax _ _ -> return vars
    F.Documentation _ -> return vars
    where
      addType = insertImportType Unqualified modName
      addValue = insertImportValue Unqualified modName

insertLocalDef :: Insert VarMap F.LocalDef
insertLocalDef vars (def, loc) =
  case def of
    F.LAnnotation _ _ -> return vars
    F.LFunc name _ _ -> insertLocalValue vars (name, loc)

separateSyntax :: Map Role (Identifier, Location) -> Map Role (Either Type Expr)
separateSyntax = Map.mapWithKey separate
  where
    separate NegateFunction (name, loc) = Right (Identifier name, loc)
    separate _ (name, loc) = Left (TCon name, loc)

desugarDefs :: ModName -> [F.Def] -> Desugar Module
desugarDefs modName defs = do
  (annos, funcs, expands, foreigns, types, synonyms) <-
    Monad.foldM addDef (Map.empty, [], [], Map.empty, Map.empty, []) defs
  let funcNames = map (\(n, _, _) -> n) funcs
  mapM_ (existsAnno funcNames) (Map.keys annos)
  let funcs' = mergeAnnos annos funcs
  syntax <- Reader.asks (separateSyntax . fst)
  return (Module funcs' expands foreigns types synonyms syntax)
  where
    addDef tuple@(annos, funcs, expands, foreigns, types, synonyms) (def, loc) =
      case def of
        F.Annotation names (tipe, _) -> do
          let names' = map (Qualified modName) names
          tipe' <- Error.annoContext names' (desugarType [] tipe)
          let keys = zipRepeat names' loc
          let newAnnos = Map.fromList (zipRepeat keys tipe') <> annos
          return (newAnnos, funcs, expands, foreigns, types, synonyms)

        F.Foreign names (tipe, _) refers -> do
          let names' = map (Qualified modName) names
          tipe' <- Error.annoContext names' (desugarType [] tipe)
          let refers' = Set.fromList refers
          let newForeigns = Map.fromList (zipRepeat names' (tipe', refers')) <> foreigns
          return (annos, funcs, expands, newForeigns, types, synonyms)

        F.Func name args body@(_, bodyLoc) -> do
          let name' = Qualified modName name
          body' <- Error.defContext name' $
            desugarExpr (F.Lambda args body, bodyLoc)
          let newFuncs = (name', body', loc) : funcs
          return (annos, newFuncs, expands, foreigns, types, synonyms)

        F.Expand name args (body, _) -> do
          let name' = Qualified modName name
          local <- Monad.foldM insertLocalValue emptyVars (zipRepeat args loc)
          body' <- Error.defContext name' $
            Reader.local (Bf.second (local <>)) (desugarExpr body)
          let newExpands = (name', args, body', loc) : expands
          return (annos, funcs, newExpands, foreigns, types, synonyms)

        F.Type name (kind, _) Nothing -> do
          let name' = Qualified modName name
          let newTypes = Map.insert name' (kind, Nothing) types
          return (annos, funcs, expands, foreigns, newTypes, synonyms)

        F.Type name (kind, _) (Just (F.Wrapper args (tipe, _) maker getter)) -> do
          let name' = Qualified modName name
          tipe' <- Error.defContext name' (desugarType args tipe)
          let maker' = Qualified modName (fst maker)
          let getter' = fmap (Qualified modName . fst) getter
          let newTypes = Map.insert name' (kind, Just (Wrapper args tipe' maker' getter')) types
          return (annos, funcs, expands, foreigns, newTypes, synonyms)

        F.Synonym name args (tipe, _) -> do
          let name' = Qualified modName name
          tipe' <- Error.defContext name' (desugarType args tipe)
          let newSynonyms = (name', args, tipe', loc) : synonyms
          return (annos, funcs, expands, foreigns, types, newSynonyms)

        F.Infix {} -> return tuple
        F.Documentation _ -> return tuple

        F.Syntax name role -> do
          (VarMap valueNames typeNames) <- Reader.asks snd
          let available = if role == NegateFunction then valueNames else typeNames
          if Qualified modName name `notElem` available
            then Error.withLocation loc (Error.notDefined name)
            else return tuple

desugarLocalDefs :: [F.LocalDef] -> Desugar [(Name, Maybe Type, Expr, Location)]
desugarLocalDefs definitions = do
  (annos, funcs) <- Monad.foldM addDef (Map.empty, []) definitions
  let funcNames = map (\(n, _, _) -> n) funcs
  mapM_ (existsAnno funcNames) (Map.keys annos)
  Arrange.recursion (mergeAnnos annos funcs)
  where
    addDef defs (def, loc) =
      case def of
        F.LAnnotation names tipe -> do
          tipe' <- desugarType [] tipe
          let keys = zipRepeat names loc
          let annos = Map.fromList (zipRepeat keys tipe')
          return $ Bf.first (annos <>) defs

        F.LFunc name args body@(_, bodyLoc) -> do
          body' <- desugarExpr (F.Lambda args body, bodyLoc)
          return $ Bf.second ((name, body', loc) :) defs

existsAnno :: (Eq a, Error.NameError a) => [a] -> (a, Location) -> Desugar ()
existsAnno funcs (name, loc)
  | name `elem` funcs = return ()
  | otherwise = Error.withLocation loc (Error.annotation name)

mergeAnnos :: Ord a => Map (a, Location) Type -> [(a, Expr, Location)] -> [(a, Maybe Type, Expr, Location)]
mergeAnnos annos = map \(name, body, loc) ->
  (name, Map.lookup name (Map.mapKeys fst annos), body, loc)

desugarExpr :: F.Expr -> Desugar Expr
desugarExpr (expr, loc) =
  case expr of
    F.Int x -> withLoc (Int x)
    F.Float x -> withLoc (Float x)
    F.Char x -> withLoc (Char x)
    F.String x -> withLoc (String x)
    F.Label x -> withLoc (Label x)

    F.Identifier name -> ifDefined name id

    F.Negate value -> do
      (syntax, vars) <- Reader.ask
      value' <- desugarExpr value
      case Map.lookup NegateFunction syntax of
        Nothing -> Error.withLocation loc Error.negateFunction
        Just (name, _) | name `elem` vars ^. valueSubs -> do
          withLoc $ Call (Identifier name, loc) value'
        Just (name, _) -> Error.withLocation loc (Error.notDefined name)

    F.Operator name Nothing Nothing ->
      ifDefined (Unqualified name) id

    F.Operator name (Just x) Nothing -> do
      x' <- desugarExpr x
      ifDefined (Unqualified name)
        \sub -> Call (sub, loc) x'

    F.Operator name Nothing (Just x) -> do
      x' <- desugarExpr x
      ifDefined (Unqualified name)
        \sub -> Lambda safeVar
          (Call (Call (sub, loc) (varRef, loc), loc) x', loc)

    F.Operator name (Just x) (Just y) -> do
      x' <- desugarExpr x
      y' <- desugarExpr y
      ifDefined (Unqualified name)
        \sub -> Call (Call (sub, loc) x', loc) y'

    F.DefIn defs body -> do
      local <- Monad.foldM insertLocalDef emptyVars defs
      Reader.local (Bf.second (local <>)) do
        defs' <- desugarLocalDefs defs
        body' <- desugarExpr body
        let defIn (n, a, b, _) x = withLoc (DefIn n a b x)
        Fold.foldrM defIn body' defs'

    F.Lambda args body -> do
      local <- Monad.foldM insertLocalValue emptyVars (zipRepeat args loc)
      Reader.local (Bf.second (local <>)) do
        body' <- desugarExpr body
        let lambda n x = withLoc (Lambda n x)
        Fold.foldrM lambda body' args

    F.Delay body -> do
      body' <- desugarExpr body
      withLoc (Lambda safeVar body')

    F.Call func arg -> do
      func' <- desugarExpr func
      arg' <- desugarExpr arg
      withLoc (Call func' arg')

    where
      withLoc x = return (x, loc)

      safeVar = "_x"
      varRef = Identifier (Unqualified safeVar)

      ifDefined name f = do
        vars <- Reader.asks snd
        case Map.lookup name (vars ^. valueSubs) of
          Nothing -> Error.withLocation loc (Error.notDefined name)
          Just sub -> withLoc $ f (Identifier sub)

desugarType :: [Name] -> F.Type -> Desugar Type
desugarType as (tipe, loc) =
  case tipe of
    F.TCon (Unqualified name) | name `elem` as ->
      return (TVar name, loc)

    F.TCon name -> ifDefined name id

    F.TOperator name x y -> do
      x' <- desugarType as x
      y' <- desugarType as y
      ifDefined (Unqualified name)
        \sub -> TCall (TCall (sub, loc) x', loc) y'

    F.TLabel x -> return (TLabel x, loc)

    F.TRecord row -> do
      (syntax, vars) <- Reader.ask
      row' <- desugarType as row
      case Map.lookup CurlyBrackets syntax of
        Nothing -> Error.withLocation loc Error.curlyBrackets
        Just (name, _) | name `elem` vars ^. typeSubs -> do
          return (TCall (TCon name, loc) row', loc)
        Just (name, _) -> Error.withLocation loc (Error.notDefined name)

    F.TVariant row -> do
      (syntax, vars) <- Reader.ask
      row' <- desugarType as row
      case Map.lookup SquareBrackets syntax of
        Nothing -> Error.withLocation loc Error.squareBrackets
        Just (name, _) | name `elem` vars ^. typeSubs -> do
          return (TCall (TCon name, loc) row', loc)
        Just (name, _) -> Error.withLocation loc (Error.notDefined name)

    F.TCall func arg -> do
      func' <- desugarType as func
      arg' <- desugarType as arg
      return (TCall func' arg', loc)

    F.TAny [] t -> desugarType as t
    F.TAny (var : vars) t -> do
      t' <- desugarType (var : as) (F.TAny vars t, loc)
      return (TAny var t', loc)

  where
    ifDefined name f = do
      vars <- Reader.asks snd
      case Map.lookup name (vars ^. typeSubs) of
        Nothing -> Error.withLocation loc (Error.notDefined name)
        Just sub -> return (f $ TCon sub, loc)

desugarModule :: Map ModName Interface -> ModName -> F.Module -> Desugar Module
desugarModule interfaces modName m = do
  vars <- Monad.foldM (insertImport interfaces) emptyVars (F.getImports m)
  vars' <- Monad.foldM (insertTopLevelDef modName) vars (F.getDefs m)
  Reader.local (Bf.second $ const vars') $
    desugarDefs modName (F.getDefs m)

desugarModules :: Bool -> Map ModName F.Module -> Either Error.Msg Module
desugarModules checkOnly modules =
  Reader.runReader (Except.runExceptT desugar) (Map.empty, emptyVars)
  where
    desugar = do
      interfaces <- mapM getInterface modules
      Monad.unless checkOnly (hasMain interfaces)
      noRepeatedInfix (concatMap F.getDefs modules)
      syntax <- getSyntaxMap (fmap F.getDefs modules)
      Fold.fold <$> Reader.local (Bf.first $ const syntax)
        (sequence $ Map.mapWithKey (desugarModule interfaces) modules)

hasMain :: Map ModName Interface -> Desugar ()
hasMain interfaces =
  case Map.lookup "Main" interfaces of
    Nothing -> Error.noMainModule
    Just (Interface funcs _) | "main" `elem` map fst funcs -> return ()
    Just _ -> Error.noMainFunction

noRepeatedInfix :: [F.Def] -> Desugar ()
noRepeatedInfix =
  Monad.foldM_ addInfix []
  where
    addInfix ops (def, loc) =
      case def of
        F.Infix name _ _
          | name `elem` ops -> Error.withLocation loc (Error.multipleInfix name)
          | otherwise -> return (name : ops)
        _ -> return ops :: Desugar [Name]

getSyntaxMap :: Map ModName [F.Def] -> Desugar SyntaxMap
getSyntaxMap modules = do
  (_, syntax) <- Monad.foldM addSyntax ([], Map.empty) defs
  return syntax
  where
    pair m ds = zip (repeat m) ds
    defs = Fold.fold (Map.mapWithKey pair modules)

    addSyntax (roles, syntax) (modName, (def, loc)) =
      case def of
        F.Syntax name role ->
          let name' = Qualified modName name in
          if role `elem` roles
            then Error.withLocation loc (Error.conflictingSyntax name')
            else return (role : roles, Map.insert role (name', loc) syntax)
        _ -> return (roles, syntax)
