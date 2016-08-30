{-# LANGUAGE ViewPatterns #-}

module Data.Monetary.BitCoin where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time

import Control.Presentation
import Control.Scan.CSV
import Data.Monetary.Currency
import Data.Monetary.USD

{--
So we've looked at graphing twitter data, and there's more to mine, given that
twitter.com gives you analytics on your tweets upon request.

We'll look at that another day.

Today: bitcoin.

I've downloaded the historical price of bitcoin vs. USD from http://bitcoinity.org

The CSV-file downloaded contains multiple exchanges but let's focus on the
last column 'others' that has the longest history.

The bitcoin price-history is at this directory and available at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M08/D25/bitcoinity_data.csv
--}

type BitCoinPrices = Map Day USD

readBitcoinPriceHistory :: FilePath -> IO BitCoinPrices
readBitcoinPriceHistory =
   fmap (Map.fromList . map dayPrice . tail . lines) . readFile

-- bitcoinity_data.csv at Data/Monetary contains a 5-yr bitcoin price history

dayPrice :: String -> (Day, USD)
dayPrice = (read . head . words . head &&& read . ('$':) . last) . csv

{--
*Y2016.M08.D25.Exercise> readBitcoinPriceHistory "Y2016/M08/D25/bitcoinity_data.csv" ~> prices
*Y2016.M08.D25.Exercise> take 3 $ Map.toList prices ~>
[(2011-08-22,$10.42),(2011-08-29,$8.57),(2011-09-05,$6.41)]

Bitcoin is bitcoin, but it does have a value to the outside world as well.

So, if you had bought 100 bitcoin,
1. 5 years ago, what would be their value today?
2. Same question, but 2 years ago?
3. Same question, but a year ago?
--}

data BitCoin = BTC Rational
   deriving (Eq, Ord)

instance Show BitCoin where
   show (BTC b) = "BTC " ++ laxmi 2 b

instance Currency BitCoin where
   value (BTC val) = val

lump :: BitCoinPrices -> BitCoin -> Maybe USD
lump prices (BTC b) =
   fmap (USD b *) (priceUSD prices (today prices))

-- where 'today' is the most recent day in BitCoinPrices

today :: Map Day a -> Day
today = last . Map.keys

{--
*Y2016.M08.D25.Exercise> lump prices (BTC 100) ~> Just $57255.99

Like I said: bitcoin is bitcoin, so it doesn't matter when you've bought it
its value today is its value today. That's a tautology.
Okay, instead, we buy $100.00 of bitcoin every month, starting from:
1. 5 years ago, or
2. 2 years ago, or
3. last year

How many bitcoins do you have today? What is their value today?
--}

monthly :: BitCoinPrices -> USD -> Day -> Maybe BitCoin
monthly prices monthlyinvestment startingdate =
   iterMos prices monthlyinvestment startingdate (today prices) (BTC 0)

iterMos :: BitCoinPrices -> USD -> Day -> Day -> BitCoin -> Maybe BitCoin
iterMos prices inv currday endday accum
        | currday > endday = Just accum
        | otherwise        =
               Map.lookupLE currday prices >>= 
               priceUSD prices . fst       >>= 
               iterMos prices inv (addMonth currday) endday . BTC
               . (value accum +) . (value inv /) . value

-- actually, the concept of 'monthly' can get messy. We'll handle this manually
-- for now, but ...

addMonth :: Day -> Day
addMonth (toGregorian -> (yr, m, d)) =
   let (yrp, newm) = m `divMod` 12
   in  fromGregorian (yr + fromIntegral yrp) (newm + 1) d

priceUSD :: BitCoinPrices -> Day -> Maybe USD
priceUSD = flip Map.lookup

-- pricesUSD gives the USD-price of bitcoin you have on fordate

bitCoinValue :: BitCoinPrices -> BitCoin -> Maybe USD
bitCoinValue prices (BTC b) =
   let todaysPrice = priceUSD prices (today prices)
   in  liftM ((*) $ USD b) todaysPrice

{--
So, for 5 years of investing:

*Y2016.M08.D25.Exercise> monthly prices (USD 100) (read "2011-08-25")
Just BTC 313.44

valued at:

*Y2016.M08.D25.Exercise> it >>= bitCoinValue prices ~>
Just $179468.35

2 years:
*Y2016.M08.D25.Exercise> monthly prices (USD 100) (read "2014-08-25")
Just BTC 7.39
*Y2016.M08.D25.Exercise> it >>= bitCoinValue prices 
Just $4233.42

1 year:
*Y2016.M08.D25.Exercise> monthly prices (USD 100) (read "2015-08-25")
Just BTC 3.23
*Y2016.M08.D25.Exercise> it >>= bitCoinValue prices 
Just $1854.54
--}
