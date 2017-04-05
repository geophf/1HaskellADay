module Y2017.M04.D05.Solution where

import Control.Arrow ((&&&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time (Day)
import Data.Time.Calendar (addDays)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

-- below modules available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import Control.Scan.CSV (csv)
import Data.Monetary.BitCoin
import Data.Monetary.Currency (value)
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

btcPrices :: FilePath -> IO BitCoinPrices
btcPrices url = Map.fromList . map toBTC . tail . lines
   <$> (simpleHTTP (getRequest url) >>= getResponseBody)

toBTC :: String -> (Day, USD)
toBTC = (read . head . words . head &&& read . ('$':) . last) . take 2 . csv

-- WAY too much duplication here for reading from an URL vice from file!

-- load in the Date,ave,max,min and return the (Day,average) pairs

{--
>>> btcPrices url
>>> btcData = it

... because of lazy IO will kill you; yes, it will!
--}

-- What is the percentage rise of BTC price from the most recent date of the
-- BTC data from the price of BTC a year ago?

annualPercentageRise :: BitCoinPrices -> Percentage
annualPercentageRise btcData =
   let latest = today btcData
       firstest = addDays (-365) latest
       late     = value (btcData Map.! latest)
   in  fromJust (P . (flip (/) <*> (late -)) . value . snd
                   <$> Map.lookupGE firstest btcData)

{--
>>> annualPercentageRise btcData 
172.48%
--}

-- What is your investment growth for

-- 1. investing a lump-sum a year ago?

-- so we need an invester algorithm; this replaces iterMos

invest :: BitCoinPrices -> USD -> [Day] -> BitCoin
invest btcData quant =
   BTC . foldr ((+) . value . btcFromUSD btcData quant) 0

-- now let's turn that into today's USD

usdValue :: BitCoinPrices -> BitCoin -> USD
usdValue btcData coins =
   fromJust (USD . (* value coins) . value <$> priceUSD btcData (today btcData))

-- and we need our lookuper and converter

btcFromUSD :: BitCoinPrices -> USD -> Day -> BitCoin
btcFromUSD btcData dollars day = 
   BTC $ fromJust ((value dollars /) . value . snd <$> Map.lookupGE day btcData)

-- now we need a set of days to invest in

investmentDays :: Int -> Integer -> Day -> [Day]
investmentDays 0 _ = const []
investmentDays n period =
   ((:) <*> investmentDays (pred n) period) . addDays (negate period)

-- and with that we run our investment scenarios:

scenario :: Int -> Integer -> BitCoinPrices -> USD -> USD
scenario n period btcData investment =
   usdValue btcData (invest btcData investment forDays)
      where forDays = investmentDays n period (today btcData)

-- so now we can do ('simply') this:

lumpSumInvestmentGrowth :: BitCoinPrices -> USD -> USD
lumpSumInvestmentGrowth = scenario 1 365

{--
>>> lumpSumInvestmentGrowth btcData (USD 12000)
$32697.66
--}

-- 2. investing monthly a set amount?

monthlyInvestmentGrowth :: BitCoinPrices -> USD -> USD
monthlyInvestmentGrowth = scenario 12 30  -- more or less

{--
>>> monthlyInvestmentGrowth btcData (USD 1000)
$20863.48

Ooh! OUCH!  :(
--}

-- 3. investing weekly a set amount?

weeklyInvestmentGrowth :: BitCoinPrices -> USD -> USD
weeklyInvestmentGrowth = scenario 52 7

{--
>>> 12000 / 52
230.76923076923077
>>> weeklyInvestmentGrowth btcData (USD 231)
$20626.05

Le sigh-a-mundo!
--}

-- 4. What is the percentage growth for each type of investment above?

percentageGrowth :: USD -> USD -> Percentage
percentageGrowth investmentAmount finalPortfolioValue =
   let (vinit, vfinal) = adjoin value (investmentAmount, finalPortfolioValue) in
   P ((vfinal - vinit) / vinit)

{--
>>> percentageGrowth (USD 12000) (lumpSumInvestmentGrowth btcData (USD 12000))
172.48%

...which matches the computed annual percentage increase in BTC price

>>> percentageGrowth (USD 12000) (monthlyInvestmentGrowth btcData (USD 1000))
73.86%
>>> percentageGrowth (USD 12000) (weeklyInvestmentGrowth btcData (USD 231))
71.88%
--}

-- Caveat: these queries are 'back-testing' data which have been notoriously
-- misused to forecast future growth. Back-testing only shows historical growth.
