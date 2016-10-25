module Y2016.M10.D25.Exercise where

import Data.Map (Map)

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658
import Analytics.Trading.Scan.Top5s     -- http://lpaste.net/1443717939034324992

{--
Okay, this sounds odd, but today we're going to do something completely 
different ... with the same data set:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

But different, none-the-less.

We're going to be focusing now, not on prices or on (solely) Leaders and Losers
but we'll be focusing on frequencies and runs, and looking at efficient 
collection methods for the same.

As before load in the data but keep it a mapping Day -> Top5s

Now, for each stock in that data set, rank the stock by the number of shows.

Hint: a stock may possibly show up more than once in a daily report.
--}

top5sShows :: FilePath -> IO (Map Symbol Int)
top5sShows = undefined

top5Show :: Archive Top5s -> Symbol -> Int
top5Show archive sym = undefined

{-- BONUS -----------------------------------------------------------------

That was easy, yes?

or was it?

Let's consider efficiency. Did you scan the top5s archive once for each stock?

Since the archive has 500+ daily entries and 1400+ stocks that a rather
expensive scan (particularly as you extend beyond just the top5s). Determine
the cost of your scan in the definition you provided to solve the above. Now,
if it wasn't a single scan of the top5s, provide a new definition.
--}

singleScanTop5sShows :: FilePath -> IO (Map Symbol Int)
singleScanTop5sShows top5s = undefined

{--
'singleScanTop5sShows' isn't a declarative name at all, but sometimes we're
paid to code FORTRAN in Smalltalk, as Kent Beck put it.
--}
