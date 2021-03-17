module Y2021.M03.D18.Exercise where

-- Remember that exercise where we built a map of keys and indices?

import Y2021.M03.D11.Solution

{--
Now, what we want to do is to have a ... okay, don't panic! ... 'mutable'
map of keys and indicies.
--}

import Data.Map (Map)

data LookupTable = LookupTable { values :: Map String Int, nextIndex :: Int }
   deriving (Eq, Ord, Show)

-- give an empty lookup table with the first-next index of 1

initLookupTable :: LookupTable
initLookupTable = undefined

-- given a preexisting map, return a lookup table with the max index + 1 as next

toLookupTable :: Map String Int -> LookupTable
toLookupTable = undefined

lookup :: String -> LookupTable -> (Int, LookupTable)
lookup key table = undefined

{--
lookup always succeeds.

If the key is in the lookup table, it returns the index with the existing
LookupTable

If the key is not in the lookup table, it inserts that key with next index,
updates next index and returns the key's index as well as the new lookup table.

What is the index of "Tom"?

What is the index of "quux"?

What is the index of "Joe"?

On a new request, is the index of "quux" still the same? ("Yes" is correct.)
--}
