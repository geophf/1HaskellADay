{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D03.Exercise where

-- Looking at the solution set of:

import Y2020.M10.D30.Solution

-- I have questions.

-- first, let's load and ... 'semi'-correct (?) those data:

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

import Y2020.M10.D12.Solution hiding (workingDir)     -- for Country
import Y2020.M10.D14.Solution                         -- for ContinentMap
import Y2020.M10.D28.Solution hiding (Alliance, name, countries) -- for Name

militaryAlliances :: IO (Map Name Alliance)
militaryAlliances =
   parseAlliances (dear ++ moderns)         >>= \allis0 ->
   countriesByContinent (workingDir ++ cbc) >>= \m ->
   let uau = updateAfricanUnion m "Africa"
                    (Alliance "African Union" Set.empty Set.empty)
   in  return (Map.insert "African Union" uau allis0)

{--
>>> militaryAlliances
...
>>> let ma = it

Looking at this map, we have an alliance, like:

>>> last (Map.elems ma)
Alliance {name = "Union State", aliases = fromList [], countries = fromList []}

An alliance of no countries. There's more than one of these. How many 'empty'
alliances are there? What are these alliances?
--}

emptyAlliances :: Map Name Alliance -> Set Name
emptyAlliances = undefined

{--
>>> let empties = emptyAlliances ma
>>> Set.size empties
14
--}

{--
The problem that created these empty alliances could be a parsing one, because
some person MIGHT have changed the data format mid-way of how an alliance is
lists, arbitrarily, because this person wanted to make my life less boring.

Thank you, some person. No, really, you're doing a great job. Love your work.

So, our task specification is a simple one:

1. determine what this new, funky format is
2. write a new, funky parser for this new alliance-format
3. go back to the file with these empty alliances and this new, funky parser
4. update the alliance map with the now-populated alliances
--}

newFunkyParser :: FilePath -> IO AllianceMap
newFunkyParser = undefined

newFunkyAllianceMap :: AllianceMap -> AllianceMap -> AllianceMap
newFunkyAllianceMap = undefined

{--
With this new, funky alliance map, answer the questions from the previous day.
Which answers have changed?
--}
