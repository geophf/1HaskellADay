module Y2016.M11.D22.Solution where

import Control.Arrow (second, (&&&))
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (getSum)
import Data.Ord

import Data.Time
import Data.Time.Clock

-- below imports available from 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import Data.Bag (fromList, rank)

import Y2016.M11.D21.Solution (daysLeft)

{--
SO MUCH happening in the month of November!

So, Niantic has announced that it is Ingress' 4th anniversary, and to commemorate
that, they are giving an award for players reaching a certain level-tier by
the end of the month. The tiers are

Don't care   bronze
Don't care   silver
13-14        gold
15           platinum
16           onyx

Now, so, let's say there's a certain player that's level twelve and has met
all the prequalifiers for level 13, so now simply needs to level up by performing
Ingress activities that score points to achieve the Gold: level 13 by the end
of the month (specifically, be at level 13 on November 30th, 2016).

Let's say the current point accumulation for this player is:
--}

type Score = Int

player1 :: Score
player1 = 11289979

-- And let's say the point level for level 13 is: 

level13 :: Score
level13 = 12000000

{-- 
Now there are activities that have point values associated for completing them.

They are as follows:
--}

type Activity = String

activities :: Map Activity Score
activities = Map.fromList [("capture portal", 1250), ("Place resonator", 625),
    ("link portals", 625), ("recharge resonator", 20), ("hack portal", 200),
    ("create control field", 2500), ("place mod", 250),
    ("destroy control field", 1250), ("destroy resonator", 375),
    ("destroy link", 375)]

-- As yesterday:
-- 1. How many points does player1 need to reach level 13?

need :: Score -> Score -> Score
need = (-)

-- *Y2016.M11.D22.Solution> need level13 player1 ~> 710021

-- 2. How many days does player1 have to reach that goal, ... given that
--    'today' is 'today'.

-- (same definition as yesterday suffices)
-- *Y2016.M11.D22.Solution> daysLeft ~> 8

-- 3. How many points per day does player1 have to maintain to reach level 13?

pointsPerDay :: ΔScore -> Days -> Score
pointsPerDay = div

-- *Y2016.M11.D22.Solution> fmap (pointsPerDay (need level13 player1) . fromIntegral) daysLeft ~> 88752

{-- BONUS -----------------------------------------------------------------

Give a set of activities that player1 should do each day to reach level 13
--}

type Days = Int
type ΔScore = Int
type Count = Int

level13DailyActivitySet :: Days -> ΔScore -> [(Count, Activity)]
level13DailyActivitySet =
   bagit . lvl13Goal (sorted (Map.toList activities)) <<- flip pointsPerDay

lvl13Goal :: [(Activity, Score)] -> ΔScore -> [Activity]
lvl13Goal [] score = [] -- error ("We have " ++ show score ++ " points to go!")
lvl13Goal list@((act, scor):rest) score =
   if score < 1 then [] else
   if scor > score then lvl13Goal rest score
   else act : lvl13Goal list (score - scor)

sorted :: [(a, Int)] -> [(a, Int)]
sorted = sortBy (compare `on` Down . snd)

bagit :: [Activity] -> [(Count, Activity)]
bagit = map (swap . second getSum) . rank . fromList

swap :: (a, b) -> (b, a)
swap = snd &&& fst
