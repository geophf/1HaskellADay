module Y2016.M08.D26.Exercise where

import Control.Monad.Writer
import Data.Map (Map)
import Data.Time

import Control.DList
import Control.Presentation (laxmi)
import Data.Monetary.Currency
import Data.Monetary.USD

import Y2016.M08.D25.Exercise

{--
Similarity? or Disparity?

Yesterday we looked at bitcoin, a currency. ("Currency" means "now"-ness)

There has been speculation that price-fluxuation of bitcoin and of gold are
similar, see, e.g.:

https://annrhefn.wordpress.com/2016/07/16/the-bitcoin-halving-quantitative-tightening-and-paving-the-road-to-mass-adoption/

Okay, that's an assertion. Let's show that there is either similarity or
disparity ... or neither ... OR BOTH! ... between gold and bitcoin, but to
show any similarity, you have to know both as a basis. Yesterday's exercise
established a 5-year basis for bitcoin. Today we will establish a 5-year
basis for gold.

Located in this directory is the historical 5-year weekly gold prices in USD,
or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M08/D26/gold-prices-usd-5-yr-weekly.csv

credit: this chart is (heavily curtailed) extracted from URL:

http://www.gold.org/research/download-the-gold-price-since-1978#package

Read in the 5-year history of gold and answer the questions similar to
yesterday's exercise for bitcoin, but this time for gold.
--}

data Gold = AU Rational deriving (Eq, Ord)

instance Show Gold where
   show (AU g) = "AU " ++ laxmi 2 g

instance Currency Gold where
   value = undefined

type GoldPrices = Map Day USD

readGoldPriceHistory :: FilePath -> IO GoldPrices
readGoldPriceHistory = undefined

-- so, with that, what is the value of an ounce of gold in USD on any given day?

pricePerOz :: GoldPrices -> Day -> Maybe USD
pricePerOz = undefined

-- say you have more than an ounce. What is the total value of your gold?

goldPrice :: GoldPrices -> Gold -> Day -> Maybe USD
goldPrice = undefined

-- what is the value of all your gold 'today,' given that today is the latest
-- day recorded in your GoldPrices-Map?

currentGoldPrice :: GoldPrices -> Gold -> Maybe USD
currentGoldPrice = undefined

-- of course, it would be helpful to formalize 'today' as a 'thing':

today :: GoldPrices -> Day
today = undefined     -- returns the last day recorded in GoldPrices

{--
With the above, answer the following questions

For a period of x years, investing $1000/month into escrow, and buying gold
when you are able (you must buy at least 3 ounce bars of gold in a purchase),

1. How many ounces of gold did you purchase?
2. How much money did you invest in these purchases? (what was your cost)
3. What is the current value of your gold?

You then should be able to determine whether gold is a profitable investment.

Answer the above 3 questions for scenarios of 5 years, 2 years, and 1 year
--}

monthlyAUinv :: GoldPrices -> USD -> Day -> Maybe Gold
monthlyAUinv prices monthlyinvest startingFrom = undefined

-- 5 year: accumulated cost, oz AU, current AU value
-- 2 year: accumulated cost, oz AU, current AU value
-- 1 year: accumulated cost, oz AU, current AU value

-- BONUS -----------------------------------------------------------------

-- Do the above, but record each purchase (of 3 oz of AU) into a log

data Purchase = Buy { oz :: Gold, costPerOz :: USD } deriving Show

auditiedMonthlyAUinv :: GoldPrices -> USD -> Day -> Writer (DList Purchase) Gold
auditiedMonthlyAUinv prices monthlyinvest startingFrom = undefined

-- BONUS-BONUS ------------------------------------------------------------

-- Also load in yesterday's bitcoin price-history, save out a CSV-file
-- that has both gold and bitcoin prices, reconciling differences in dates,
-- if any. Chart bitcoin's prices vs. gold's

chartBTCvsAU :: BitCoinPrices -> GoldPrices -> FilePath -> IO ()
chartBTCvsAU btc au savetofilenamed = undefined
