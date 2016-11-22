module Y2016.M11.D22.Exercise where

import Data.Time
import Data.Map (Map)
import qualified Data.Map as Map

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
activities = Map.fromList [("capture portal", 1250), ("Place resinator", 625),
    ("link portals", 625), ("recharge resinator", 20), ("hack portal", 200),
    ("create control field", 2500), ("place mod", 250),
    ("destroy control field", 1250), ("destroy resinator", 375),
    ("destroy link", 375)]

-- As yesterday:
-- 1. How many points does player1 need to reach level 13?

-- 2. How many days does player1 have to reach that goal, ... given that
--    'today' is 'today'.

-- 3. How many points per day does player1 have to maintain to reach level 13?

{-- BONUS -----------------------------------------------------------------

Give a set of activities that player1 should do each day to reach level 13
--}

type Days = Int
type ΔScore = Int
type Count = Int

level13DailyActivitySet :: Days -> ΔScore -> [(Count, Activity)]
level13DailyActivitySet daysLeft scoreDiff = undefined
