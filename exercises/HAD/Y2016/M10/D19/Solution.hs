module Y2016.M10.D19.Solution where

import Data.Map (Map)
import qualified Data.Map as Map

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658
import Analytics.Trading.Scan.Top5s     -- http://lpaste.net/1443717939034324992

-- Below imports are available from 1HaskellADay git repository

import Data.Bag
-- import Data.MultiMap (MultiMap)
-- import qualified Data.MultiMap as MM

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

type BS = Bag Symbol
type MBS = Maybe BS
type AccMap = Map LeadersLosers BS

countsInCategories :: FilePath -> IO AccMap
countsInCategories =
   fmap (foldr processEachTop5s Map.empty . Map.elems) . top5sHistory

processEachTop5s :: Top5s -> AccMap -> AccMap
processEachTop5s tops = flip (foldr (uncurry addLine)) (Map.toList tops)

-- for each map, an entry is a category with leaders and losers, so we add
-- that line to accumulating multimap

addLine :: Top5Kind -> ([Symbol], [Symbol]) -> AccMap -> AccMap
addLine cat (leaders, losers) mm =
   adding Leader cat leaders (adding Loser cat losers mm)

adding :: (Top5Kind -> LeadersLosers) -> Top5Kind -> [Symbol] -> AccMap -> AccMap
adding kind cat syms = Map.alter (nothingXOr (flip (foldr add) syms)) (kind cat)

nothingXOr :: (BS -> BS) -> MBS -> MBS
nothingXOr f Nothing = Just (f emptyBag)
nothingXOr f (Just bag) = Just (f bag)

{--
*Y2016.M10.D19.Solution> countsInCategories "Y2016/M10/D17/top5s.csv" ~> ans
*Y2016.M10.D19.Solution> head (Map.toList ans) ~>
(Leader Mkt_Cap,fromList [("AA",Sum {getSum = 2}),("AAPL",Sum {getSum = 141}),...
*Y2016.M10.D19.Solution> length . Map.keys . snd $ head (Map.toList ans) ~> 232

Huh! There is a LOT of data here. Let's analyze it. Tomorrow.
--}
