{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Data.MultiMap where

-- so I just got tired of writing the insertWith function on Map

-- or: a MultiMap that can have multiple values for one key
-- a simple approach is Map k [v]

import Prelude hiding (lookup)

import Control.Arrow
import Control.Monad.Writer
import qualified Data.Foldable as Fld
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

data MultiMap k a m = MM { store :: Map k m, fn :: a -> m }

instance (Show k, Show m) => Show (MultiMap k a m) where
   show = ("MM " ++) . show . store

dup :: a -> (a,a)
dup x = (x, x)

instance (Ord k, Monad m, Monoid (m a)) => Monoid (MultiMap k a (m a)) where
   mempty = empty
   m1@(MM map1 fn1) `mappend` m2@(MM map2 _ignored) =
      MM (Map.fromList (map (dup >>> second (merger map1 map2)) 
                        (Set.toList (keysSet m1 `Set.union` keysSet m2)))) fn1

merger :: (Ord k, Monoid v) => Map k v -> Map k v -> k -> v
merger map1 map2 key = n2m (key, map1) `mappend` n2m (key, map2)

-- and now that multimaps are monoidal, we can do THIS:

record :: (Ord v, Ord k, Monad w, Monoid (w v)) =>
          k -> v -> Writer (MultiMap k v (w v)) ()
record key val = tell (fromList return [(key, val)])
      -- which is the same for Maps, too

-- Functions ------------------------------------------------------------

-- Prototype: empty (based on Set a monoid) -----------------------------

empty :: (Monad m, Monoid (m a)) => MultiMap k a (m a)
empty = MM Map.empty return

-- Retrievals: keysSet, lookup ------------------------------------------

keysSet :: MultiMap k a m -> Set k
keysSet (MM map _) = Map.keysSet map

elems :: Monoid m => MultiMap k a m -> m
elems = mconcat . map snd . toList

lookup :: (Monoid m, Ord k) => k -> MultiMap k a m -> m
lookup k (MM map _) = fromMaybe mempty (Map.lookup k map)

size :: MultiMap k a m -> Int
size (MM map _) = Map.size map

-- Modifiers: insert, delete, mergeValues --------------------------------

insert :: (Ord k, Monoid m) => k -> a -> MultiMap k a m -> MultiMap k a m
insert k v (MM map lift) =
   MM (Map.insertWith mappend k (lift v) map) lift

delete :: Ord k => k -> MultiMap k a m -> MultiMap k a m
delete key (MM map fn) = MM (Map.delete key map) fn

mergeValues :: (Ord k, Monoid m) => k -> k -> MultiMap k a m -> MultiMap k a m
mergeValues idx1 idx2 (MM map fn) =
   MM (Map.insert idx1 (n2m (idx1, map) `mappend` n2m (idx2, map)) map) fn

-- n2m: Nothing-to-mempty ... yeah, yeah: I know, but what can you do? :/

n2m :: (Ord k, Monoid m) => (k, Map k m) -> m 
n2m = fromMaybe mempty . uncurry Map.lookup

-- Conversions: toList, fromList ----------------------------------------

toList :: MultiMap k a m -> [(k, m)]
toList (MM m _) = Map.toList m

toPairs :: Foldable m => MultiMap k a (m a) -> [(k, a)]
toPairs = concatMap (sequence . fmap Fld.toList) . toList
-- from Diego Roque @__gcd for #1liner challenge on @1HaskellADay

fromList :: (Monoid (m a), Ord k) => (a -> m a) -> [(k, a)]
            -> MultiMap k a (m a)
fromList liftFn = foldr (uncurry insert) (MM Map.empty liftFn)

-- example and test:

testMM :: (MultiMap Int String [String], [[String]])
testMM = let m = execWriter (record 1 "dog" >> record 2 "cat" >>
                             record 1 "goldie") -- as in 'fish'
             -- m1 = insert 1 "dog" m
             -- m2 = fromList [(2, "cat")] `mappend` m1
             -- m3 = insert 1 "goldie" m2 -- as in 'fish'
             lookups = map (flip lookup m) [1, 7, 2]
         in  if lookups /= [["dog", "goldie"], [], ["cat"]]
             then error ("MultiMap test failed! mm: " ++ show m)
             else (m, lookups)
