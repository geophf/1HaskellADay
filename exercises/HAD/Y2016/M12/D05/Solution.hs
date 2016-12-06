module Y2016.M12.D05.Solution where

import Control.Arrow ((&&&), second)
import Control.Comonad ((=>>), extract)
import qualified Data.Map as Map
import Data.Time

-- below import available via 1HaskellADay git repository

import Control.List  -- for weave and List Comonad
import Control.Logic.Frege ((<<-))
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

sma :: (Fractional a, Num a) => Int -> [a] -> a
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

Apply both SMA 12 and SMA 26 to a year of the historical BitCoin prices. What 
are the results that you see?

The historical BitCoin prices are available at the URL 

https://api.blockchain.info/charts/market-price?timespan=52weeks&rollingAverage=24hours&format=csv

Or a recent snapshot is available on this repository at:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M12/D01/btcprices.csv

(including the mean gain prices, which you may ignore if you wish)
--}

btcSMA12and26 :: FilePath -> BitCoinPrices -> IO ()
btcSMA12and26 outfile =
   writeFile outfile . unlines . ("Date,BTC Price,SMA 12, SMA 26" :)
                     . map tuples2Str . computeSMAs . Map.toList

-- Actually, price HISTORY analyses goes FORWARD through time, which means
-- it goes BACKWARDS through the list of chronologically-ascending values

type PriceSMAs = (USD, (USD, USD))

computeSMAs :: [(Day, USD)] -> [(Day, PriceSMAs)]
computeSMAs = uncurry zip . second (reverse . computeSMAs' . reverse) . unzip

-- the computation over the list is simply an extension of the SMA

computeSMAs' :: [USD] -> [PriceSMAs]
computeSMAs' list = list =>> (extract &&& (sma 12 &&& sma 26))

tuples2Str :: Show a => (a, PriceSMAs) -> String
tuples2Str (d,(u,(sma12,sma26))) = weave (show d:map show [u,sma12,sma26])

{--
*Y2016.M12.D05.Solution> btcPriceHistoryFromURL "Y2016/M12/D01/market-price-btc.csv" >>= btcSMA12and26 "Y2016/M12/D05/btcSMAs12and26.csv"

The ol' reverse-compute-reverse-trick worked! AHA!

You see, looking at the resulting chart, that the SMA 12 follows the data-points
more closely and is therefore more reactive, and the SMA 26 follows the data-
points, but in a more sluggish manner and is therefore the 'smoother' curve.
--}
