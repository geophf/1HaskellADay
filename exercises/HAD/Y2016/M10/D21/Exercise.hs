module Y2016.M10.D21.Exercise where

import Data.Map (Map)

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658
import Analytics.Trading.Scan.Top5s     -- http://lpaste.net/1443717939034324992

-- the below imports are available from 1HaskellADay git repository

import Graph.ScoreCard
import Y2016.M10.D19.Exercise (LeadersLosers)

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

top5s2ScoreCards :: FilePath -> IO (Map Symbol (ScoreCard Symbol LeadersLosers Int))
top5s2ScoreCards top5sfile = undefined
