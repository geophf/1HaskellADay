{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D04.Exercise where

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
import Y2020.M10.D30.Exercise
import Y2020.M11.D03.Exercise

import Data.Set (Set)

{--
Load in the military alliances from file. We have a parsed set of alliances.

Now, write function that finds just the alliance name in a line. Collect those
alliances. Compare them to the alliances we read in previous exercises.

How many alliances did we miss? What are their names? Write an enhancement
that adds those alliances to our AllianceMap.
--}

missingAlliances :: FilePath -> IO (Set Name)
missingAlliances = undefined

enhance :: AllianceMap -> AllianceMap
enhance = undefined
