module Control.Map where

-- some common map utilities

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

snarf :: (Ord c, Ord d) => (a -> Maybe (c,d)) -> [a] -> Map c (Set d)
snarf f = foldr inserter Map.empty . mapMaybe f

inserter :: (Ord a, Ord b) => (a, b) -> Map a (Set b) -> Map a (Set b)
inserter (a, b) m =
   Map.insert a (maybe (Set.singleton b) (Set.insert b) (Map.lookup a m)) m
