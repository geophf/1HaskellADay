{-# LANGUAGE ViewPatterns, TupleSections #-}

module Y2016.M08.D26.Solution where

import Control.Arrow ((&&&), first)
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time

-- these modules are available in this, the 1HaskellADay, git repository

import Control.DList
import Control.List (weave)
import Control.Logic.Frege ((<<-))
import Control.Presentation (laxmi)
import Control.Scan.CSV (csv)
import Data.Monetary.Currency
import Data.Monetary.USD

import Y2016.M08.D25.Solution

data Gold = AU Rational deriving (Eq, Ord)

instance Show Gold where
   show (AU g) = "AU " ++ laxmi 2 g

instance Currency Gold where
   value (AU g) = g

instance Num Gold where
   AU a + AU b = AU (a + b)
   AU a * AU b = AU (a * b)
   abs (AU a) = AU (abs a)
   fromInteger a = AU (fromInteger a)
   negate (AU a) = AU (negate a)
   signum (AU a) = AU (signum a)

type GoldPrices = Map Day USD

readGoldPriceHistory :: FilePath -> IO GoldPrices
readGoldPriceHistory =
   fmap (Map.fromList . map line2GoldPrice . tail . lines) . readFile

line2GoldPrice :: String -> (Day, USD)
line2GoldPrice = (read . head &&& read . ('$':) . last) . csv

{--
*Y2016.M08.D26.Solution> readGoldPriceHistory "Y2016/M08/D26/gold-prices-usd-5-yr-weekly.csv" ~> golds ~> length ~> 262
--}

-- so, with that, what is the value of an ounce of gold in USD on any given day?

pricePerOz :: GoldPrices -> Day -> Maybe USD
pricePerOz = flip Map.lookup

-- say you have more than an ounce. What is the total value of your gold?

goldPrice :: GoldPrices -> Day -> Gold -> Maybe USD
goldPrice golds dayo (AU oz) = fmap (USD oz *) (pricePerOz golds dayo)

-- what is the value of all your gold 'today,' given that today is the latest
-- day recorded in your GoldPrices-Map?

currentGoldPrice :: GoldPrices -> Gold -> Maybe USD
currentGoldPrice prices = goldPrice prices (today prices)

-- of course, it would be helpful to formalize 'today' as a 'thing':

{--
today :: GoldPrices -> Day
today = last . Map.keys

today is already defined in yesterday's solution
--}

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

type Portfolio m a = m (a, USD)

monthlyAUinv :: MonadPlus m => GoldPrices -> USD -> Day -> Portfolio m Gold
monthlyAUinv prices monthlyinvest startingFrom =
   let zero = USD 0 in fmap (first oz) $
   maui return prices monthlyinvest zero zero startingFrom (today prices) (AU 0)

addmos :: Day -> Day
addmos (toGregorian -> (yr, m, d)) =
   let (yrp, newm) = m `divMod` 12
   in  fromGregorian (yr + fromIntegral yrp) (newm + 1) d

-- sees if we can buy gold, and if we can, buy it (3 ounces or more)
cantbuymelove :: Day -> USD -> USD -> (Purchase, USD)
cantbuymelove day reserve costperoz =
   let (USD num, USD dom) = (reserve, costperoz)
       goldoz = fromIntegral (floor (num / dom))
       remain = num - goldoz * dom
       mkpurch oz = Buy (AU oz) costperoz day in
   if   num >= 3 * dom
   then (mkpurch goldoz, USD remain)
   else (mkpurch 0, reserve)

{--
Article: Unwinding a recursive function; Deconstructing recursion

Say you have a specialization of a fold:

maui :: GoldPrices -> USD -> USD -> USD -> Day -> Day -> Gold -> Maybe (Gold, USD)
maui prices permos avail toteinv day today accum
     | day > today = return (accum, toteinv)
     | otherwise   =
   Map.lookupLE day prices >>= (\(bought, left) ->
   maui prices permos left (permos + toteinv) (addmos day) today (accum + bought))
   . cantbuymelove (avail + permos) . snd

el yawno, and so what?

The problem here is that this function is a specialized form of loop (in 
Common Lisp it's pronounced LOOP), when, really, loop is a generalized-form,
and the specialization is what you are doing in the loop. AND, suppose you want
to do some more stuff down the line? What are you going to do in that case?
Copy-and-paste this function and insert the additional functionality in betwixt?

I THINK NOT

Okay, let's unwind this function. What does this function do?
1a. WORK: COMPUTE requested day's gold purchase, if any
1b.       ACCUMULATE total invested/gold store
2. INDUCTION STEP: advance requested day, 
3. RECURSE

simple enough. Well, then, let's decompose the maui function do to these things.
Also, let's generalize around the monad. Maybe, itself, is too specific,
especially if we're going to use a writer monad down the road. Why do we have
to be stuck on just Maybe?
--}

buy :: Monad m => GoldPrices -> USD -> Day -> Portfolio m Purchase
buy prices avail day =
   maybe2m (fmap (cantbuymelove day avail . snd) (Map.lookupLE day prices))

maybe2m :: Monad m => Maybe a -> m a
maybe2m (Just a) = return a
-- maybe2m _        = mzero

{-- the buy fn with the maybe2m-helper simplifies the original maui fn to:

maui :: MonadPlus m => GoldPrices -> USD -> USD -> USD -> Day -> Day -> Gold -> m (Gold, USD)
maui prices permos avail toteinv day today accum
     | day > today = return (accum, toteinv)
     | otherwise   = buy prices (avail + permos) day >>= \(bought, left) ->
   maui prices permos left (permos + toteinv) (addmos day) today (accum + bought)

The maui fn is now simplified, and has a more general MonadPlus-type class
value returned, but it is not generalized enough. Why?

What if I want to do something whilst looping? How do I insert functionality
(or 'inject' functionality) into the loop? Aspects? Well, yes, but we call them
continuations here. Let's generalize maui to the CPS: continuation passing style
--}

type CPS m a = (a, USD) -> Portfolio m a

maui :: Monad m => CPS m Purchase -> GoldPrices -> USD -> USD -> USD -> Day
                -> Day -> Gold -> Portfolio m Purchase
maui pass prices permos avail toteinv day today accum
     | day > today = return (Buy accum (USD 0) day, toteinv)
     | otherwise   =
   buy prices (avail + permos) day >>= pass >>= \(Buy bought _ _, left) ->
   maui pass prices permos left (permos + toteinv) 
              (addmos day) today (accum + bought)

-- Great! So now we have a function that yields a generalized monadic type
-- and allows us to insert functionality at each step of the loop. Let's proceed

{-- 5 year: accumulated cost, oz AU, current AU value

*Y2016.M08.D26.Solution> let (Just (fiveyrAU, inv)) = monthlyAUinv prices (USD 1000) (read "2011-08-27")
*Y2016.M08.D26.Solution> currentGoldPrice prices fiveyrAU ~> Just $56549.01
*Y2016.M08.D26.Solution> it >>= return . ((-) inv) ~> Just $3450.98

Which is actually NEGATIVE $3.5k because (-) needs to be flipped. Ouch.

-- 2 year: accumulated cost, oz AU, current AU value

*Y2016.M08.D26.Solution> let (Just (twoyrAU, inv)) = monthlyAUinv prices (USD 1000) (read "2014-08-27")
*Y2016.M08.D26.Solution> currentGoldPrice prices twoyrAU ~> Just $24235.29
*Y2016.M08.D26.Solution> fmap (flip (-) inv) it ~> Just $235.29

eh.

-- 1 year: accumulated cost, oz AU, current AU value

*Y2016.M08.D26.Solution> let (Just (oneyrAU, inv)) = monthlyAUinv prices (USD 1000) (read "2015-08-27")
*Y2016.M08.D26.Solution> currentGoldPrice prices oneyrAU ~> Just $12117.64
*Y2016.M08.D26.Solution> fmap (flip (-) inv) it ~> Just $117.64

eh, eh!
--}

-- BONUS -----------------------------------------------------------------

-- Do the above, but record each purchase (of 3 oz of AU) into a log

data Purchase = Buy { oz :: Gold, costPerOz :: USD, onDate :: Day } deriving Show

type WriterM = Writer (DList Purchase)

-- in this case the Writer is the monad

cont :: CPS (WriterM ) Purchase
cont buy@(b@(Buy (AU au) _ _), _) = when (au > 0) (tell $ dl' b) >> return buy

-- so we just call maui with the continuation function:

auditiedMonthlyAUinv :: GoldPrices -> USD -> Day
                     -> Portfolio (WriterM ) Purchase
auditiedMonthlyAUinv prices monthlyinv startingFrom =
   let zero = USD 0 in
   maui cont prices monthlyinv zero zero startingFrom (today prices) (AU 0)

{--

Here's the purchases over five years, auditted:

*Y2016.M08.D26.Solution> let (ans, tron) = runWriter (auditiedMonthlyAUinv prices (USD 1000) (read "2011-08-26"))
*Y2016.M08.D26.Solution> mapM_ print $ dlToList tron
Buy {oz = AU 3.00, costPerOz = $1606.50, onDate = 2011-12-26}
Buy {oz = AU 3.00, costPerOz = $1569.50, onDate = 2012-05-26}
Buy {oz = AU 3.00, costPerOz = $1716.00, onDate = 2012-10-26}
Buy {oz = AU 3.00, costPerOz = $1607.75, onDate = 2013-03-26}
Buy {oz = AU 3.00, costPerOz = $1331.00, onDate = 2013-07-26}
Buy {oz = AU 3.00, costPerOz = $1246.25, onDate = 2013-11-26}
Buy {oz = AU 3.00, costPerOz = $1336.00, onDate = 2014-03-26}
Buy {oz = AU 3.00, costPerOz = $1294.75, onDate = 2014-07-26}
Buy {oz = AU 3.00, costPerOz = $1232.75, onDate = 2014-10-26}
Buy {oz = AU 3.00, costPerOz = $1208.25, onDate = 2015-02-26}
Buy {oz = AU 3.00, costPerOz = $1170.50, onDate = 2015-06-26}
Buy {oz = AU 3.00, costPerOz = $1146.65, onDate = 2015-09-26}
Buy {oz = AU 3.00, costPerOz = $1068.25, onDate = 2015-12-26}
Buy {oz = AU 3.00, costPerOz = $1243.25, onDate = 2016-04-26}

WOOT!
--}

-- BONUS-BONUS ------------------------------------------------------------

-- Also load in yesterday's bitcoin price-history, save out a CSV-file
-- that has both gold and bitcoin prices, reconciling differences in dates,
-- if any. Chart bitcoin's prices vs. gold's

-- something like this:

mergeDayMaps :: Ord k => Map k a -> Map k b -> [(k, (a,b))]
mergeDayMaps m1 =
   mapMaybe (\(k,v2) -> fmap ((k,) . (,v2) . snd) (Map.lookupLE k m1))
   . Map.toList

chartBTCvsAU :: FilePath -> BitCoinPrices -> GoldPrices -> IO ()
chartBTCvsAU outfile =
   writeFile outfile . unlines . ("Date,BitCoin,Gold":) . map asCSV <<- mergeDayMaps

asCSV :: (Show a, Show b, Show c) => (a,(b,c)) -> String
asCSV (a,(b,c)) = weave [show a, show b, show c]

{--
*Y2016.M08.D26.Solution> readGoldPriceHistory "Y2016/M08/D26/gold-prices-usd-5-yr-weekly.csv" ~> goldo
*Y2016.M08.D26.Solution> readBitcoinPriceHistory "Y2016/M08/D25/bitcoinity_data.csv" ~> bitto
*Y2016.M08.D26.Solution> chartBTCvsAU "Y2016/M08/D26/bitcoin-vs-gold-5yrs.csv" bitto goldo

Saved at this directory and charted on solution tweet.

The correllation between gold and bitcoin over five years seems to me that
there is no correllation between the two. Do you see a correllation?
--}
