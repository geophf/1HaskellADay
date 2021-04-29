{-# LANGUAGE ViewPatterns #-}

module Y2021.M04.D28.Solution where

{--
This is interesting

>>> Nothing > Just 70
False

>>> Nothing < Just 70
True

... huh. So Nothing is ... Just 0? That, for me, isn't good. I don't want
something to succeed when I have only partial data. 

Pesky me.

So, we have some functions that take two values, dependent on both, and
some other functions, that, to conform to the interface, takes two values,
but only depends on the first value.

Why would I do that?

Trending data and analytics. Some analytics depend on a trend, established
over a time series, and some analytics are realized from current data (only).

Take indicators from the stock market, for example. Some indicators, like the
SMA50/200 (Simple Moving Averages 50 (days) and 200 (days)) require a cross-
over from the previous period to this period:

buy = yesterday (sma50 < sma200) && today (sma50 > sma200)

Whereas the Relative Strength Index has a buy or sell signal dependent only
on a cross-over in this period

buy = today (rsi14 < 30)
sell = today (rsi14 > 70)

But you may not always have trending data, for example: from a new security.

Let's do this.

You have a Time-series of trending data.
--}

import Control.Arrow (second)
import Control.Monad (liftM2)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Map (snarf)  -- lol

data Trend = Trend { sma50, sma200, rsi14 :: Maybe Double }
   deriving (Eq, Ord, Show)

-- note: you may have some, or all, trending data.

-- And you have function-type that takes trending data and returns a buy/sell
-- recommendation

data Call = BUY | SELL
   deriving (Eq, Ord, Show)

data Basis = GoldenCross | DeathCross | RSI70 | RSI30
   deriving (Eq, Ord, Show)

data Recommendation = Rec Call Basis
   deriving (Eq, Ord, Show)

type RunRec = Trend -> Trend -> Maybe Recommendation

{--
Today's Haskell problem is a two-layered cake (with icing, of course):

First, the sma functions need both today's and yesterday's trends (as described
above). The RSI functions need only today's trends.

ANY function will fail if the indicator in the trend is not present.

From a time-series of trends, collect and return buy/sell recommendations.

DISCLAIMER! Please note, all data provided are FAKE, MADE UP, INACCURATE.

A BUY or SELL recommendation here is NOT financial advice in any way, shape,
or form, so ... THERE!

OKAY!

So, let's pretend we have three companies. One that has one set of trending 
data, and the other two that have a series of trending data, complete or
incomplete.

The timeseries goes into the past: the first element is today's trends, the
second element is yesterday's trends, third, day before's, ... etc.
--}

abc, xyz, pqr :: [Trend]
abc = [Trend (Just 7.4) Nothing (Just 55)]
xyz = [Trend (Just 10) (Just 20) (Just 27), Trend (Just 11) Nothing (Just 12)]
pqr = [Trend (Just 7) (Just 15) (Just 45), Trend (Just 17) (Just 14) (Just 59)]

-- Now we have recommendation functions

-- to set up, we need functions that compare data in the monadic domain

lt, gt :: Maybe Double -> Maybe Double -> Maybe Bool
lt = liftM2 (<)
gt = liftM2 (>)

nd :: Maybe Bool -> Maybe Bool -> Maybe Bool
nd = liftM2 (&&)

-- now, define implication:

(-|) :: Bool -> a -> Maybe a
True -| q = Just q
False -| _ = Nothing

-- now: using (-|) create (=|)

(=|) :: Maybe Bool -> a -> Maybe a
p =| q = p >>= (-| q)

-- the 'Golden Cross' says SMA50 crosses above the SMA200 from yesterday to
-- today.

goldenCross :: RunRec
goldenCross today yesterday =
   (sma50 today `gt` sma200 today)
    `nd` (sma50 yesterday `lt` sma200 yesterday) =| Rec BUY GoldenCross

-- The 'Death Cross' says the dual

deathCross :: RunRec
deathCross today yesterday =
   (sma50 today `lt` sma200 today)
    `nd` (sma50 yesterday `gt` sma200 yesterday) =| Rec SELL DeathCross

-- The RSI 14 cases:

rsi70, rsi30 :: RunRec
rsi70 today _ = rsi14 today `gt` Just 70 =| Rec SELL RSI70
rsi30 today _ = rsi14 today `lt` Just 30 =| Rec BUY RSI30

-- now the rsi-functions can be run straight off, but the cross-functions
-- need to be lifted:

type RunRec' = Trend -> Maybe Trend -> Maybe Recommendation

-- write a function to convert an RunRec-function to a RunRec' one.

toRunRec' :: RunRec -> RunRec'
toRunRec' f t0 mbt1 = mbt1 >>= f t0

-- run the functions against the companies and return a set of recommendations
-- (if any) for each company

type Company = String

runRecs :: [RunRec] -> [(Company, [Trend])] -> Map Company (Set Recommendation)
runRecs (map toRunRec' -> recs) = 
   Map.fromList
   . filter (not . (== Set.empty) . snd)
   . map (second (flip runRec recs))

runRec :: [Trend] -> [RunRec'] -> Set Recommendation
runRec (t0:ts) = Set.fromList . mapMaybe (flip uncurry (t0, listToMaybe ts))

recs :: [RunRec]
recs = [goldenCross, deathCross, rsi70, rsi30]

-- what are the Buy or Sell recommendations for abc, xyz, and pqr?

{--
>>> runRecs recs (zip (words "abc xyz pqr") [abc, xyz, pqr])
fromList [("pqr",fromList [Rec SELL DeathCross]),("xyz",fromList [Rec BUY RSI30])]
--}
