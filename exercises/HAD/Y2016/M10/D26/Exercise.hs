module Y2016.M10.D26.Exercise where

import Data.Map (Map)
import Data.Time

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658
import Analytics.Trading.Scan.Top5s     -- http://lpaste.net/1443717939034324992

-- below imports available from 1HaskellADay git repository

import Y2016.M10.D19.Exercise (LeadersLosers)

{--
Yesterday we counted occurences of a security in the Top5s daily reports. Great!
That information is useful, and an indicator of staying power. Another such
indicator is runs of a stock.

Today's Haskell problem.

Load the top5s daily archive from the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

Now let's compute some runs.
--}

-- 1. What are all the runs (ordered by longest first) of all the stocks?
-- A run is successive daily showings in the top5s lists

{--

type Run = Int

top5sRuns :: Archive Top5s -> Map Symbol [Run]

nope! nope! nope! That isn't very helpful at all! What if I wish to investigate
a run of a stock in the Top5s? Where do I start looking if I don't know the
dates of the run?

Starting over:
--}

data Run = Run { ndays :: Int, from, to :: Day }
   deriving Show

-- n.b. ndays IS NOT diffDays to from (weekends and holidays don't count)

top5sRuns :: Archive Top5s -> Map Symbol [Run]
top5sRuns top5s = undefined

-- there!

-- 2. Now, let's do more focused runs. If a stock is hi-lo-hi-lo that's one 
-- thing but if it's hi-hi-hi-hi, that's quite another thing. We're going to 
-- focus runs on leaders or losers, then: leaders or losers by category

top5sLeadersRuns, top5sLosersRuns :: Archive Top5s -> Map Symbol [Run]
top5sLeadersRuns top5s = undefined
top5sLosersRuns top5s = undefined

-- You may think a losers' run is ignominious, but much good analysis (and
-- profitability) can be gained by following the losers, too.

-- 3. And, finally the BAC ('Bank of America') case. Some stocks excell in a
-- particular category. What is the Leaders Volume runs for BAC?

top5sLeadersByCategoryRuns :: Archive Top5s
                           -> Map Symbol (Map LeadersLosers [Run])
top5sLeadersByCategoryRuns top5s = undefined
