module Y2016.M08.D30.Exercise where

-- below imports available on 1HaskellADay git repository

import Control.Scan.CSV
import Data.Monetary.BitCoin
import Data.Monetary.USD

{--
Okay, last week we started looking at the bitcoin value-proposition a little
bit, comparing it to itself and then bitcoin against gold.

But how do you get bitcoins? One way is to buy them, which is a perfectly
viable option (which we will explore further), another way is to earn them,
by exchanging work for bitcoin (you know, like, money, and stuff), and another
way is to mine them.

Bitcoin mining is a whole, rich topic into itself, including how mining evolved
to the point now where mining uses specialized hardware with chips designed
with that sole-purpose. The internet is available for you to explore this
topic further.

So, let's pretend we know all about mining. Just go with me on this one for now.

Okay, as a miner, you ... can use your computer, and that worked in the early
days of mining, but now: not so much. Let's buy some mining hardware and then
start to mine.

But which mining hardware? Going to

https://www.bitcoinmining.com/bitcoin-mining-hardware/

three options are recommended, so let's look at those.
--}

data PowerEfficiency = SomeMeasureOfEfficiency
   deriving (Eq, Ord, Show)

data BitCoinMiner =
   BTCMiner { model :: String,
              cost :: USD,
              efficiency :: PowerEfficiency,
              approxBTCperMonth :: BitCoin }
      deriving (Eq, Ord, Show)

readBitCoinMiners :: FilePath -> IO [BitCoinMiner]
readBitCoinMiners = undefined

-- read in the models provided from bitcoin_miners.csv in this directory, url:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M08/D30/bitcoin_miners.csv

-- Hint: perhaps Control.Scan.CSV.csv will help?

-- Now that you have those data. What is the best machine to do bitcoin
-- mining? Why? Come up with a set of measures that compare cost, efficiency
-- and approximate bitcoins mined to determine what's the 'best' machine

data CostEfficiencyBTCMeasure = MeasureYouDetermine

bestBitCoinMiner :: [BitCoinMiner] -> (BitCoinMiner, CostEfficiencyBTCMeasure)
bestBitCoinMiner = undefined

-- Show your justification for the 'best' bitcoin miner
