module Y2017.M04.D05.Exercise where

import Data.Time
import Network.HTTP

-- below modules available via 1HaskellADay git repository

import Control.Scan.CSV
import Data.Monetary.BitCoin
import Data.Monetary.USD
import Data.Percentage

{--
In this tweet

https://twitter.com/ohiobitcoin/status/849368023588909057

@ohiobitcoin claims that the Ukraine increased its investment in bitcoin by
500%. Wow. 500%. But how much did bitcoin rise over the past year?

Today's Haskell problem.

Download the daily bitcoin price per dollar over the last year, then determine
its percentage increase.
--}

url :: FilePath
url = "http://data.bitcoinity.org/export_data.csv?currency=USD"
   ++ "&data_type=price&exchange=kraken&r=day&t=l&timespan=2y"

-- the above URL gives 2 years of BitCoin data as price per USD

btcPrices :: FilePath -> BitCoinPrices
btcPrices url = undefined

-- load in the Date,ave,max,min and return the (Day,average) pairs

-- hint: Data.Monetary.BitCoin functions MAY help.

-- What is the percentage rise of BTC price from the most recent date of the
-- BTC data from the price of BTC a year ago?

annualPercentageRise :: BitCoinPrices -> Percentage
annualPercentageRise btcData = undefined

-- What is your investment growth for

-- 1. investing a lump-sum a year ago?

lumpSumInvestmentGrowth :: BitCoinPrices -> USD -> USD
lumpSumInvestmentGrowth btcData investment = undefined

-- 2. investing monthly a set amount?

monthlyInvestmentGrowth :: BitCoinPrices -> USD -> USD
monthlyInvestmentGrowth btcData monthlyInvest = undefined

-- 3. investing weekly a set amount?

weeklyInvestmentGrowth :: BitCoinPrices -> USD -> USD
weeklyInvestmentGrowth btcData weeklyInvest = undefined

-- 4. What is the percentage growth for each type of investment above?

percentageGrowth :: USD -> USD -> Percentage
percentageGrowth initialAmt finalPortfolioValue = undefined

-- Caveat: these queries are 'back-testing' data which have been notoriously
-- misused to forecast future growth. Back-testing only shows historical growth.
