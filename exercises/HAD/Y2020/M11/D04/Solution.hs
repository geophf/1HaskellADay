{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D04.Solution where

{--
So. I posted the following in yesterday's solution:

HEY!

>>> Map.lookup (T.pack "Union of South American Nations") ma1
Nothing

... we have more work to do :/

This is the 'work to do'-exercise.

What is the work we are going to do?

Yesterday we found which alliances where poorly parsed (yielding an empty
Country set), but we didn't find which alliances weren't parsed successfully
at all. Let's find those missing alliances today.
--}

import Y2020.M10.D28.Solution hiding (Alliance, name, countries) -- for Name
import Y2020.M10.D30.Solution
import Y2020.M11.D03.Solution

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

{--
Load in the military alliances from file. We have a parsed set of alliances.

Now, write function that finds just the alliance name in a line. Collect those
alliances. Compare them to the alliances we read in previous exercises.

How many alliances did we miss? What are their names? Write an enhancement
that adds those alliances to our AllianceMap.
--}

type LineNumber = Int
type AllianceIndex = Map Name (Set LineNumber)

missingAlliances :: FilePath -> IO AllianceIndex
missingAlliances file = parseEachLine 1 Map.empty . lines <$> readFile file

parseEachLine :: LineNumber -> AllianceIndex -> [String] -> AllianceIndex
parseEachLine _ ans [] = ans
parseEachLine ln acc (h:t) =
   parseEachLine (succ ln) (parseLine ln acc (snd $ break (== '[') h)) t

unionSample :: String
unionSample = concat ["*{{Flagicon|UNASUR}} [[Union of South American Nations]]",
     " - Includes Venezuela, Uruguay, Bolivia, Guyana and Suriname, with Peru ",
     "being a suspended member of UNASUR."]

{--
>>> break (=='[') unionSample 
("*{{Flagicon|UNASUR}} ","[[Union of South American Nations]] - Includes ...")
--}

parseLine :: LineNumber -> AllianceIndex -> String -> AllianceIndex
parseLine _ ans [] = ans
parseLine ln acc brackies@(_:_) = inserter key ln acc
   where key = T.pack . fst . break (==']') $ drop 2 brackies

inserter :: Ord k => Ord v => k -> v -> Map k (Set v) -> Map k (Set v)
inserter k v m =
   Map.insert k (maybe (Set.singleton v) (Set.insert v) (Map.lookup k m)) m

{--
>>> parseEachLine 15 Map.empty [unionSample]
fromList [("Union of South American Nations",fromList [15])]

Woot!

With this, we should get a set of Alliances-..ish from the file to compare 
against our previously-parsed result-set.

>>> missingAlliances (dear ++ moderns)
fromList [("2001 Sino-Russian Treaty of Friendship",fromList [60]), ...]
>>> let missin = it
>>> Map.size missin
53

... bit of a misnomer, because we have to see what we have verses what we don't
to get to a true `missin`.

>>> militaryAlliances
fromList [("2001 Sino-Russian Treaty of Friendship",Alliance {...})]
>>> let ma0 = it
>>> let ma = refinements ma0

>>> let diff = Set.difference (Map.keysSet missin) (Map.keysSet ma)
>>> take 3 $ Set.toList diff
>>> Set.size diff
35

Well, that's disheartening that we missed 35 Alliances. Maybe some of these
are spurious, but we have to dig further before we can make that claim, ...

... and fix the ones that are legitimately missing, anyway.

It looks like we have our work cut out for us.

enhance :: AllianceMap -> AllianceMap
enhance = undefined

... which we will do.

... tomorrow.
--}
