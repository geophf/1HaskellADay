module Y2016.M12.D06.Solution where

import Control.Arrow ((&&&))

-- below imports available from 1HaskellADay git repository

import Data.Monetary.BitCoin

import Y2016.M12.D01.Solution
import Y2016.M12.D05.Solution

{--
Yesterday we looked at the SMA/Simple Moving Average as a trend-indicator, and
we saw that it is a more responsive indicator than using simply the mean gain

... a much more responsive indicator.

In theory, and in practice, the more responsive an indicator is the faster
one can react to 'where the data are going.' The more able you are to react,
the better your decisions are ... in theory. And, it turns out, in practice,
too... that is: based on your decision-making, bad decisions usually lead to
bad results. YMMV.

So, SMA has good responsiveness. Good enough?

Hm.

The more responsive you can be, the (even more) responsive you find you need
to be. So, is there a more-responsive technical indicator

1. Why, yes, and
2. there are several (and more than several) indicators other than SMA that
may be more apt for decision-making on your data sets.

Today, let's look at one of them: the EMA, or Exponential Moving Average.

The SMA/Simple Moving Average is 'responsive' over the last n data points, but
evenly so. What if timely is more important, so the most recent data point is
the most relevant. The EMA/Exponential Moving Average, also called the Weighted
Moving Average, addresses this: it gives the most weight to the most recent
data point, the second-most weight to the previous data point, on, as you can
presume, in an exponentially-diminished set of weights.

Now, I can write all about the EMA, but there's a lovely site that does this 
already. I'll let you read that, and we'll write the code, in Haskell, here.

http://www.investopedia.com/articles/trading/10/simple-exponential-moving-averages-compare.asp?ad=dirN&qo=investopediaSiteSearch&qsrc=0&o=40186

Okay, so let's distill the article.

The EMA is computed thus:

EMA = (P * α) + (Previous EMA * (1 - α))

    where P = current data-point
          α = smoothing factor = 2 / ( 1 + n)
          n = number of data points in a period

So, in Haskell, you define:
--}

ema :: Fractional a => Int -> a -> [a] -> a
ema n prevEMA = (prevEMA * (1 - α) +) . (α *) . head -- given the list never ends
   where α = 2.0 / (1.0 + fromIntegral n)

-- define ema

-- question: is this the best approach, type-wise? As EMA is defined recursively
-- should ema's declaration be Int -> [a] -> [a]? Or should the previous EMA
-- be stored in, e.g. the State-monad? What is a good number for the first
-- previous EMA? How do we handle that first case in the function definition?

-- Thoughts on this from type-theorists?

-- Now for the BitCoin Price history, chart the historical prices against
-- EMA 12 and EMA 26, as you did yesterday for SMA

btcEMA12and26 :: FilePath -> BitCoinPrices -> IO ()
btcEMA12and26 = btcAnalyses "EMA 12,EMA 26" computeEMAs

-- starter-function kicks off the computation with the first value as seed

computeEMAs :: Fractional a => [a] -> [Indicators a]
computeEMAs = uncurry computeEMAs' . ((head &&& head) &&& id)

-- the worker-function inlines EMA computations into the time-series

computeEMAs' :: Fractional a => (a, a) -> [a] -> [Indicators a]
computeEMAs' _ [] = []
computeEMAs' (p12, p26) list@(h:t) =
   let nuance = (ema 12 p12 &&& ema 26 p26) list in
   (h, nuance):computeEMAs' nuance t

{--
The computeEMAs' function is a stateful mapping over the price-history.

Reminder: The historical BitCoin prices are available from the URL:

http://www.investopedia.com/articles/trading/10/simple-exponential-moving-averages-compare.asp?ad=dirN&qo=investopediaSiteSearch&qsrc=0&o=40186

or I've taken a snapshot at:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M12/D01/market-price-btc.csv

You can get the BitCoinPrices value by using Y2016.M12.D01.Exercise.btcPriceHistoryFromURL

so:

*Y2016.M12.D06.Solution> 
   btcPriceHistoryFromURL "Y2016/M12/D01/market-price-btc.csv" >>=
   btcEMA12and26 "Y2016/M12/D06/btcEMA12and26.csv"

Shows the EMA 12 and EMA 26 indicators in action. Rather boring-looking. The
interesting bits come to play in the interplay of the indicators, themselves.

But that is for another day.
--}
