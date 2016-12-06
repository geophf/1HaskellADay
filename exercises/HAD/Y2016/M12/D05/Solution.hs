module Y2016.M12.D05.Solution where

import Control.Arrow ((&&&), second)
import Control.Comonad ((=>>), extract)
import qualified Data.Map as Map
import Data.Time

-- below import available via 1HaskellADay git repository

import Control.List  -- for weave and List Comonad
import Control.Logic.Frege ((<<-), adjoin)
import Data.Monetary.BitCoin
import Data.Monetary.USD

import Y2016.M12.D01.Solution

{--
So 'yesterday' (Friday) we saw the solution of mean gain of BitCoin to be
'okay' in that it started the same and ended the same as the actual price.

But there's a danger here: either we are fitting the curve to the results
(the danger is that if the actual data trend changes, the average is slow
to adjust to that change), or, as just hinted, our anticipation is slow.

A colleague of mine said: "it's like driving a car looking only at the rear-
view mirror."

Two thoughts:

1. yes, that's an apt analogy
2. but what are the results?

You drive your car like that, you crash. You do forecasting like that ...?

Do you crash?

Yes, and no. Data scientists, market forecasters, analysts all have been looking
at this problem a long time, and have come up with more (and more (and more))
ways to look at data to model, then to forecast trends.

The finding, generally, is that the sooner you can anticipate a trend, the
sooner you can make decisions on that trend.

Makes sense.

So, the last trender was a mean-gain analytic tool. The problem there was that
you needed the whole data set to make a decision. Not a problem for most data
sets these days, but it makes forecasting dicey if you don't know you're in a
reversal of the general trend.

So, let's look at more responsive trending analytical tools.

Today we are going to look at the SMA: the Simple Moving Average.

The SMA does this: it takes the last n data-points of raw data ('raw data' is
redundant, btw), to render the next, estimated, point. For each new data-point
it follows the same process.

--}

sma :: Fractional a => Int -> [a] -> a
sma = uncurry (/) . (sum &&& fromIntegral . length) <<- take

-- as you can see from the signature, sma is comonadic, and that's how we'll
-- use it.

{--
So, for an sma 3, it takes the last three data-points to estimate the next, or
'new', value.

As you can see, sma is a class of estimators. What are good 'n's for sma-
estimators. Well, that's hard to say without experience as a guide. In the
stock markets, analysts agree that SMA 12 and SMA 26 are both good forecasting
tools.

CORRECTION: SMA 15 and 50 are considered to be good indicators

Apply both SMA 15 and SMA 50 to a year of the historical BitCoin prices. What 
are the results that you see?

The historical BitCoin prices are available at the URL 

https://api.blockchain.info/charts/market-price?timespan=52weeks&rollingAverage=24hours&format=csv

Or a recent snapshot is available on this repository at:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M12/D01/btcprices.csv

(including the mean gain prices, which you may ignore if you wish)
--}

-- Actually, price HISTORY analyses goes FORWARD through time, which means
-- it goes BACKWARDS through the list of chronologically-ascending values

{--
btcSMA15and50 :: FilePath -> BitCoinPrices -> IO ()
btcSMA15and50 outfile =
   writeFile outfile . unlines . ("Date,BTC Price,SMA 15,SMA 50" :)
                     . map tuples2Str . project computeSMAs . Map.toList
--}

-- a generalization of the above function for any paired technical analyses:

btcAnalyses :: String -> ([USD] -> [Indicators USD]) -> FilePath -> BitCoinPrices -> IO ()
btcAnalyses indicatorNames indfns outfile =
   writeFile outfile . unlines . (("Date,BTC Price," ++ indicatorNames) :)
                     . map tuples2Str . project indfns . Map.toList

-- allowing us to write the processor function:

btcSMA15and50 :: FilePath -> BitCoinPrices -> IO ()
btcSMA15and50 = btcAnalyses "SMA 15,SMA 50" computeSMAs

type Indicators a = (a, (a, a))

{--
computeSMAs :: [(Day, USD)] -> [(Day, PriceSMAs)]
computeSMAs = uncurry zip . second (reverse . computeSMAs' . reverse) . unzip
--}

-- computeSMAs, as written, is actually a metafunction that formats the data 
-- for use by the 'actual' function to do the work. Let's, then, generalize
-- this function. And I overthought projecting forward through time. As these
-- data points are chronologically-arrayed, we are already projecting forward
-- so the reverse . f . reverse contortions are unnecessary (and wrong).

project :: ([n] -> [m]) -> [(a, n)] -> [(a, m)]
project f = uncurry zip . second f . unzip

-- project makes the very big assumption that the input list is a chronologically-
-- ordered time-series. We control this by Map.toList on the BitCoinPrices
-- value, but we could also require this by tightening up the types so that
-- second f means what we say, not what we hope.

-- But then do we get TOO specific with the types and render project unusable
-- for whole classes of problems? This is my conundrum.

-- the computation over the list is simply an extension of the SMA

{--
computeSMAs :: [USD] -> [Indicators USD]
computeSMAs list = list =>> (extract &&& (sma 15 &&& sma 50))
--}

-- but this, too, is a specialization of a generalized function:

extend2 :: ([a] -> a) -> ([a] -> a) -> [a] -> [Indicators a]
extend2 f g history = history =>> (extract &&& (f &&& g))

-- so then we simply write here:

computeSMAs :: Fractional a => [a] -> [Indicators a]
computeSMAs = uncurry extend2 (adjoin sma (15, 50))

-- Then provide the results in printable form (CSV-format)

tuples2Str :: (Show a, Show b) => (a, Indicators b) -> String
tuples2Str (d,(u,(lo,hi))) = weave (show d:map show [u,lo,hi])

{--
*Y2016.M12.D05.Solution> 
   btcPriceHistoryFromURL "Y2016/M12/D01/market-price-btc.csv" >>=
   btcSMA15and50 "Y2016/M12/D05/btcSMAs15and50.csv"

The ol' reverse-compute-reverse-trick worked! AHA!

You see, looking at the resulting chart, that the SMA 15 follows the data-points
more closely and is therefore more reactive, and the SMA 50 follows the data-
points, but in a more sluggish manner and is therefore the 'smoother' curve.
--}
