{-# LANGUAGE ViewPatterns #-}

module Data.MemoizingTable where

{--
Captures the idea that you get information on your data sets incrementally. So
you start with the known lookups, add the ones you don't know yet, refine those,
then add the refined lookups to the known sets.
--}

import Prelude hiding (init)

import Control.Arrow ((&&&))
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

-- Our memoizing table, partitioning what we know against what we do know

data MemoizingTable a b =
   MT { fromTable :: Map a b, readIndex :: Map b a, newValues :: Set b }
      deriving Show

type MemoizingState m a key val =
   StateT (MemoizingTable key val, Map key [val]) m a

-- creates a new memoizing table

init :: Ord a => Ord b => (Map a b, Map b a) -> MemoizingTable a b
init = flip (uncurry MT) Set.empty

-- partitions a datum: do we know it? If not, add it to new information
-- for later processing

triage :: Ord a => Ord b => b -> MemoizingTable a b -> MemoizingTable a b
triage k (MT mapi mapk n00b) =
   MT mapi mapk ((if containsKey k mapk then id else Set.insert k) n00b)
      where containsKey k = Set.member k . Map.keysSet

-- triaging with state

type MemoizingS m a b c = StateT (MemoizingTable a b, Map a [b]) m c

triageM :: Monad m => Ord a => Ord b => a -> [b] -> MemoizingS m a b ()
triageM idx vals = get >>= \(mt0, reserve) ->
   put ((foldr triage mt0 &&& flip (Map.insert idx) reserve) vals)
 
-- When we get indices for the new information, we update the memoizing table

update :: Ord a => Ord b => [(a,b)] -> MemoizingTable a b -> MemoizingTable a b
update (bifurcate -> (mi, mk)) (MT mi' mk' _) =
   init (merge mi mi', merge mk mk')
      where merge m1 = Map.fromList . (Map.toList m1 ++) . Map.toList

bifurcate :: Ord a => Ord b => [(a,b)] -> (Map a b, Map b a)
bifurcate = (Map.fromList &&& Map.fromList . map swap)

-- To start of a memoizing table from a single list:

start :: Ord a => Ord b => [(a, b)] -> MemoizingTable a b
start = init . bifurcate
