{-# LANGUAGE TupleSections #-}

module Y2021.M03.D18.Solution where

-- Remember that exercise where we built a map of keys and indices?

import Y2021.M03.D11.Solution

{--
Now, what we want to do is to have a ... okay, don't panic! ... 'mutable'
map of keys and indicies.
--}

import Data.List (maximum)

import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (lookup)

data LookupTable =
   LookupTable { values :: Map String Integer, nextIndex :: Integer }
      deriving (Eq, Ord, Show)

-- give an empty lookup table with the first-next index of 1

initLookupTable :: LookupTable
initLookupTable = LookupTable Map.empty 1

-- given a preexisting map, return a lookup table with the max index + 1 as next

toLookupTable :: Map String Integer -> LookupTable
toLookupTable = LookupTable <*> succ . maximum . Map.elems

lookup :: String -> LookupTable -> (Integer, LookupTable)
lookup key l@(LookupTable m idx) =
   maybe (idx, LookupTable (Map.insert key idx m) (succ idx))
         ((,l)) (Map.lookup key m)

{--
>>> let lk = toLookupTable (uniqueIds names)

lookup always succeeds.

If the key is in the lookup table, it returns the index with the existing
LookupTable

If the key is not in the lookup table, it inserts that key with next index,
updates next index and returns the key's index as well as the new lookup table.

What is the index of "Tom"?

>>> lookup "Tom" lk
(1,LookupTable {values = fromList [("Dick",2),("Harry",3),("Tom",1)], nextIndex = 4})
>>> let lk1 = snd it

What is the index of "quux"?

>>> lookup "quux" lk1
(4,LookupTable {values = fromList [("Dick",2),("Harry",3),("Tom",1),("quux",4)], nextIndex = 5})
>>> let lk2 = snd it

What is the index of "Joe"?

>>> let (joeIdx, lk3) = lookup "Joe" lk2
>>> joeIdx 
5

On a new request, is the index of "quux" still the same? ("Yes" is correct.)

>>> fst $ lookup "quux" lk3
4
--}
