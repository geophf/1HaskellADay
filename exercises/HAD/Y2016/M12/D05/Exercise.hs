module Y2016.M12.D05.Exercise where

-- below import available via 1HaskellADay git repository

import Y2016.M12.D01.Exercise

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

sma :: Num a => Int -> [a] -> a
sma period historicalData = undefined

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

btcSMA12and26 :: FilePath -> IO ()
btcSMA12and26 btcRawPrices = undefined
