module Y2016.M10.D19.Exercise where

import Data.Map (Map)

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658
import Analytics.Trading.Scan.Top5s     -- http://lpaste.net/1443717939034324992

-- Below imports are available from 1HaskellADay git repository

import Data.Bag

{--
Let's continue our analyses that we started yesterday on the stock top5s,
recorded at:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

One way to find patterns in data is to look for specific things in the data,
as we did yesterday, where we saw the patterns of losers-then-leaders and
leaders-then-losers in the Price category.

Another way to find patterns is by clustering. We've looked at K-means clustering
before, back in March of this year: http://lpaste.net/3576182129349885952

We're NOT going to be doing clustering today, because what we have is a 'morass'
of data, not a set of ScoreCards, or vectors of attributes.

So, today, let's turn the morass of data that we do have into a set of 
ScoreCards, or a set of vectors of attributes.

Today is Step 1 of that.

Today. We won't look at adjacency at all. For each stock, count the number
of shows in each category: Price, Market Capitalization, and Volume, further
distinguishing the stock by losers and leaders in the appropriate categories.

So, a ScoreCard for AAPL might look like: 

SC "AAPL" [(Price Leaders, ...

Well, what does it look like? That entails quite a bit of work, so let's break
it down even further.

Today's Haskell problem is even simpler than creating score cards.

Today: load in the top5s.csv from the URL above

Then, count all occurrences of all stocks for each category.
--}

data LeadersLosers = Leader Top5Kind | Loser Top5Kind
   deriving (Eq, Ord, Show)

countsInCategories :: FilePath -> Map LeadersLosers (Bag Symbol)
countsInCategories top5sfile = undefined

-- hint: use top5sHistory to load in the history file.
-- n.b.: Volume Top5Kind has only leaders, not losers.

-- Tomorrow we'll look at generating score cards from these category counts
