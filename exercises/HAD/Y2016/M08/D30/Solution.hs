{-# LANGUAGE ViewPatterns #-}

module Y2016.M08.D30.Solution where

import Control.Arrow ((&&&), (>>>))
import Data.List (sort)

-- below imports available on 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import Control.Presentation (laxmi)
import Control.Scan.CSV (csv)
import Data.Monetary.BitCoin
import Data.Monetary.Currency
import Data.Monetary.USD

data PowerEfficiency = Effic { eff :: Float }
   deriving (Eq, Ord, Show)

data BitCoinMiner =
   BTCMiner { model :: String,
              cost :: USD,
              efficiency :: PowerEfficiency,
              approxBTCperMonth :: BitCoin }
      deriving (Eq, Ord, Show)

readBitCoinMiners :: FilePath -> IO [BitCoinMiner]
readBitCoinMiners = fmap (map toMiner . tail . lines) . readFile

toMiner :: String -> BitCoinMiner
toMiner (csv -> [mod,cost,eff,btc]) =
   BTCMiner mod (read cost) (readEffic eff)
            (BTC (toRational $ ((read btc ) :: Float)))

readEffic :: String -> PowerEfficiency
readEffic = Effic . read . head . words

{--
*Y2016.M08.D30.Solution> readBitCoinMiners "Y2016/M08/D30/bitcoin_miners.csv" ~> miners
[BTCMiner {model = "AntMiner S7", cost = $479.95,
           efficiency = Effic 0.25, approxBTCperMonth = BTC 0.16},
 BTCMiner {model = "Avalon6", cost = $499.95, efficiency = Effic 0.29, 
           approxBTCperMonth = BTC 0.12},
 BTCMiner {model = "SP20 Jackson", cost = $248.99, efficiency = Effic 0.65, 
           approxBTCperMonth = BTC 0.05}]
--}

-- Now that you have those data. What is the best machine to do bitcoin
-- mining? Why? Come up with a set of measures that compare cost, efficiency
-- and approximate bitcoins mined to determine what's the 'best' machine

data Measure = Measure Rational
   deriving (Eq, Ord)

instance Show Measure where
   show (Measure m) = laxmi 4 m

type CostPerBTC = Measure

computeCostPerBTC :: BitCoinMiner -> CostPerBTC
computeCostPerBTC = realize rats2meas (value . approxBTCperMonth) (value . cost)

type PowerCost = Measure

computePowerCost :: BitCoinMiner -> PowerCost
computePowerCost =
   realize rats2meas (value . cost) (toRational . eff . efficiency)

rats2meas :: Rational -> Rational -> Measure
rats2meas = Measure <<- (/)

realize :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d

-- isn't this some Theorem of Logic? It feels quite like the S-combinator!

realize r f g = f &&& g >>> uncurry r

data CostEfficiencyBTCMeasure =
   BTCMinerMeasure { costPerBTC :: CostPerBTC, powerCost :: PowerCost }
      deriving (Eq, Show)

instance Ord CostEfficiencyBTCMeasure where
   compare a b = compare (costPerBTC b) (costPerBTC a)

measure :: BitCoinMiner -> CostEfficiencyBTCMeasure
measure = realize BTCMinerMeasure computeCostPerBTC computePowerCost

type RankedMiner = (CostEfficiencyBTCMeasure, BitCoinMiner)

bestBitCoinMiner :: [BitCoinMiner] -> RankedMiner
bestBitCoinMiner = head . rankedBitCoinMiners

{--
*Y2016.M08.D30.Solution> bestBitCoinMiner miners ~>
(BTCMinerMeasure {costPerBTC = 0.00, powerCost = 1919.81},
 BTCMiner {model = "AntMiner S7", cost = $479.95, 
           efficiency = Effic {eff = 0.25}, approxBTCperMonth = BTC 0.16})
--}

-- let's actually show all of them ranked:

rankedBitCoinMiners :: [BitCoinMiner] -> [RankedMiner]
rankedBitCoinMiners = sort . map (measure &&& id)

{--
*Y2016.M08.D30.Solution> mapM_ print $ rankedBitCoinMiners miners
(BTCMinerMeasure {costPerBTC = 0.0003, powerCost = 1919.8199},
 BTCMiner {model = "AntMiner S7", cost = $479.95, efficiency = Effic {eff = 0.25}, approxBTCperMonth = BTC 0.16})
(BTCMinerMeasure {costPerBTC = 0.0002, powerCost = 1723.9828},
 BTCMiner {model = "Avalon6", cost = $499.95, efficiency = Effic {eff = 0.29}, approxBTCperMonth = BTC 0.12})
(BTCMinerMeasure {costPerBTC = 0.0002, powerCost = 383.0692},
 BTCMiner {model = "SP20 Jackson", cost = $248.99, efficiency = Effic {eff = 0.65}, approxBTCperMonth = BTC 0.05})
--}
