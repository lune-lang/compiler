{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Compiler.Arrange (recursion, noRecursion, withRefers) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Bifunctor as Bf
import Data.Function ((&))

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set, (\\))

import Control.Monad.Except (ExceptT)

import qualified Compiler.Error as Error
import Syntax.Common
import Syntax.Desugared

type Arrange a = forall m. (Monad m) => ExceptT Error.Msg m a

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

class Expression a where
  isConstant :: a -> Bool

instance Expression Expr where
  isConstant (expr, _) =
    case expr of
      Lambda _ _ -> False
      _ -> True

instance Expression Type where
  isConstant _ = False

class (Variable a, Expression b) => FreeVars a b where
  freeVars :: b -> Set a

instance Variable a => FreeVars a Expr where
  freeVars (expr, _) =
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

instance FreeVars Identifier Type where
  freeVars (tipe, _) =
    case tipe of
      TCon n -> Set.singleton n
      TVar _ -> Set.empty
      TLabel _ -> Set.empty
      TCall func arg -> freeVars func <> freeVars arg
      TAny _ t -> freeVars t

getOrder :: Variable a => Bool -> Map a (Set a, Location) -> Arrange [a]
getOrder allowRecursion refs
  | Map.null refs = return []
  | Set.null roots = Error.withLocation loc recursionError
  | otherwise = (Set.toList roots ++) <$> getOrder allowRecursion rest
  where
    loc = snd $ snd $ head (Map.toList refs)

    recursionError
      | allowRecursion = Error.mutualRecursion
      | otherwise = Error.synonymRecursion

    isRoot n (rs, _)
      | allowRecursion = Set.null rs || Set.toList rs == [n]
      | otherwise = Set.null rs

    roots = Map.keysSet (Map.filterWithKey isRoot refs)
    rest = Bf.first (\\ roots) <$> foldr Map.delete refs roots

setConcatMap :: (Ord a) => (a -> Set a) -> Set a -> Set a
setConcatMap f = Set.fold (<>) Set.empty . Set.map f

internalVars
  :: (FreeVars a b)
  => Map a (Set a)
  -> [(a, c, b, Location)]
  -> b
  -> Set a
internalVars refers funcs t =
  setConcatMap vars (freeVars t)
  where
    names = map (\(n, _, _, _) -> n) funcs

    vars n
      | n `elem` names = Set.singleton n
      | isConstant t, Just refs <- Map.lookup n refers = setConcatMap vars refs
      | otherwise = Set.empty

arrange
  :: (FreeVars a b)
  => Bool
  -> Map a (Set a)
  -> [(a, c, b, Location)]
  -> Arrange [(a, c, b, Location)]
arrange allowRecursion refers funcs = do
  order <- getOrder allowRecursion referenceMap
  let position name = Maybe.fromMaybe 0 (List.elemIndex name order)
  return $ List.sortOn (position . nameOf) funcs
  where
    nameOf (n, _, _, _) = n

    vars = internalVars refers funcs

    referenceMap =
      map (\(n, _, t, loc) -> (n, (vars t, loc))) funcs
      & Map.fromList

recursion, noRecursion
  :: (FreeVars a b)
  => [(a, c, b, Location)]
  -> Arrange [(a, c, b, Location)]
recursion = arrange True Map.empty
noRecursion = arrange False Map.empty

withRefers
  :: (FreeVars a b)
  => Map a (Set a)
  -> [(a, c, b, Location)]
  -> Arrange [(a, c, b, Location)]
withRefers = arrange True
