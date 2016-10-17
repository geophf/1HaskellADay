module Y2016.M10.D17.Exercise where

import Data.Map (Map)
import Data.Set (Set)
import Data.Time

-- below import available from 1HaskellADay git repository

import Control.Scan.CSV

{--
In observing the top 5s of the stock markets, I've started to see some patterns
in securities over time that may be interesting.

Do you see the same things?

Today's Haskell exercise. There is a file of the top5s securities of the stock
markets included here at this directory as top5s.csv (it's not truly csv) or
at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

Today, we're interested in looking at the leaders and the losers in the Price
category over a period of days.

Read in the file, extract the stocks for the prices, losers and leaders, and
answer the below
--}

type Stock = String

data Prices = Price { leaders, losers :: Set Stock }
   deriving Show

-- so, read in the price line, remembering the date for it (a few lines up)
-- and then output a map of days -> prices

type Hist = Map Day Prices

readPrices :: FilePath -> Hist
readPrices file = undefined

-- Now.

-- what is the distribution of stocks and their appearances on the top5s prices?

type NStocks = Int
type NDays = Int

dist :: Hist -> (NStocks, NDays)
dist priceHistory = undefined

-- What stocks show up on the leaders one day and the losers the next day?

leadThenLose :: Hist -> [Stock]
leadThenLose priceHistory = undefined

-- Of course, you need a 'next trading day' defined, because Saturdays, Sundays
-- and holidays (some of them) are not traded, therefore not recorded

nextTradingDay :: Hist -> Day -> Day
nextTradingDay hist today = undefined

-- Opposite question: which stocks show up on the losers bracket then the winners?

loseThenLead :: Hist -> [Stock]
loseThenLead hist = undefined

-- what is the frequency of appearance of these flippin' stocks?

showings :: Hist -> Stock -> Int
showings hist stock = undefined

-- that is to say, does a leadThenLose show up frequently on the top5s list?
-- How about a loseThenLead stock?
