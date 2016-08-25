module Y2016.M08.D25.Exercise where

import Data.Map (Map)
import Data.Time

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
readBitcoinPriceHistory = undefined

{--
Bitcoin is bitcoin, but it does have a value to the outside world as well.

So, if you had bought 100 bitcoin,

1. 5 years ago, what would be their value today?
2. Same question, but 2 years ago?
3. Same question, but a year ago?
--}

data BitCoin = BTC Rational
   deriving (Eq, Ord, Show)

instance Currency BitCoin where
   value = undefined

lump :: BitCoinPrices -> BitCoin -> Day -> USD
lump prices coins purchaseDate = undefined -- gives the value in USD 'today'

-- where 'today' is the most recent day in BitCoinPrices

{--
Okay, instead, we buy $100.00 of bitcoin every month, starting from:

1. 5 years ago, or
2. 2 years ago, or
3. last year

How many bitcoins do you have today? What is their value today?
--}

monthly :: BitCoinPrices -> USD -> Day -> BitCoin
monthly prices monthlyinvestment startingdate = undefined

priceUSD :: BitCoinPrices -> Day -> BitCoin -> USD
priceUSD prices fordate ncoins = undefined

-- pricesUSD gives the USD-price of bitcoin you have on fordate
