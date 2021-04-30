module Data.LookupTable where

-- Gives the index for a lookup value (useful for storing data from a lookup)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import Data.Tuple (swap)

type LookupTable = Map String Integer

-- Now we need to flip the table for when we're doing fetches

type LookDown a = Map Integer a

lookdown :: Ord a => Map a Integer -> LookDown a
lookdown = Map.fromList . map swap . Map.toList

-- but what's the next index?

nextIndex :: Ord a => Map a Integer -> Integer
nextIndex = succ . maybe 0 fst . Set.maxView . Map.keysSet . lookdown

{--
>>> nextIndex Map.empty
1

BOOM! ... well, ... kinda boom
--}
