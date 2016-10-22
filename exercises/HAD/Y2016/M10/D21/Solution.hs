{-# LANGUAGE ViewPatterns #-}

module Y2016.M10.D21.Solution where

import Control.Arrow (second, (&&&))
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (getSum)

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658
import Analytics.Trading.Scan.Top5s     -- http://lpaste.net/1443717939034324992

-- the below imports are available from 1HaskellADay git repository

import Data.Bag
import Graph.ScoreCard
import Y2016.M10.D19.Solution

{--
Wow! Years, months, and days of exercises for 1HaskellADay. That's something.

Okay, enough musing! Onto today's problem.

Yesterday, you were able to convert the record of each day's top 5s stocks-by-
category into a record of, well, top5s by category, but in this case, it was a
mapping

category type -> stock-and-count

transformed from the original

date -> category -> (leaders stocks, losers stocks)

Today we (can) use what we did yesterday to transform the top5s stored at:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

into a mapping of scorecards:

Symbol -> ScoreCard Symbol LeadersLosers Int

Let's do that.
--}

type SCSyms = ScoreCard Symbol LeadersLosers Int
type MSyms a = Map Symbol a

top5s2ScoreCards :: FilePath -> IO (MSyms SCSyms)
top5s2ScoreCards =
    fmap (Map.mapWithKey SC
        . foldr (uncurry addCatForSyms) Map.empty
        . Map.toList)
  . countsInCategories

-- so we just flip keys and values and distribute the keys over the values

type ALI = Array LeadersLosers Int

addCatForSyms :: LeadersLosers -> Bag Symbol -> MSyms ALI -> MSyms ALI
addCatForSyms val (map (second getSum) . Map.toList -> keys) mm =
   foldr (\(k, v) -> Map.alter (updateOneVal v) k) mm (redistribute (val,keys))

{--
How did I arrive at the above line?

Actually, addCatForSyms is too big for this bear of a little brain. So, let's
break it down.

To update one value of the map (an array), we have
let updateOneVal arr = (arr //) . pure

But we don't know of the array exists yet. That's why we have mt (below). So
we need to conditionally update a pre-existing array or provide an empty array
if none pre-exists.

old code:
   flip (foldr (Map.alter (xOr ((//) . pure) mt))) mm (redistribute (val, keys))
--}

updateOneVal :: (LeadersLosers, Int) -> Maybe ALI -> Maybe ALI
updateOneVal kv Nothing = updateOneVal kv (Just mt)
updateOneVal kv (Just arr) = Just (arr // pure kv)

-- updateOneVal makes a lovely fold

redistribute :: (v, [(k, x)]) -> [(k, (v, x))]
redistribute = map (fst . snd &&& (fst &&& snd . snd)) . sequence

-- Now, since the domain is so small, I could just count each value explicitly
-- for the to/from enum values, but the underlying types are Enum-like enough
-- that I can just apply the product and sum types directly

instance Enum LeadersLosers where
   fromEnum (Leader x) = fromEnum x * 2
   fromEnum (Loser  x) = fromEnum x * 2 + 1
   toEnum x = let (y, z) = divMod x 2 in
      (if z == 1 then Loser else Leader) $ toEnum y

instance Ix LeadersLosers where
   range (a, b) = [a .. b]
   inRange (a, b) c = fromEnum c >= fromEnum a && fromEnum b >= fromEnum c
   index (a, b) c = fromEnum c - fromEnum a

mt :: ALI
mt = listArray (Leader Mkt_Cap, Leader Volume) (repeat 0)

{-- So:
*Y2016.M10.D21.Solution> top5s2ScoreCards "Y2016/M10/D17/top5s.csv" ~> tops
*Y2016.M10.D21.Solution> length tops ~> 1447
*Y2016.M10.D21.Solution> tops Map.! "AAPL" ~>
SC {idx = "AAPL", values = array (Leader Mkt_Cap,Leader Volume)
                                 [(Leader Mkt_Cap,141),(Loser Mkt_Cap,67),
                                  (Leader Price,0),(Loser Price,0),
                                  (Leader Volume,208)]}

Next week we'll look at frequencies and runs for securities in the market.
--}
