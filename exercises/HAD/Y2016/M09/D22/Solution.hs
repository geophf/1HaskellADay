module Y2016.M09.D22.Solution where

import Control.Arrow ((&&&), (>>>))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

-- the below imports available from 1HaskellADay git repository

import Data.BlockChain.Block.Blocks
import Data.BlockChain.Block.Summary
import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types

import Data.Monetary.BitCoin

{--
*Cue long, exasperated sigh from geophf, followed by a rant:

I hate it when I compose a 'simple' Haskell problem, like: 'view a Merkle tree
of some BitCoin transactions,' and the solution entails twenty-or-so times the
effort I had originally anticipated.

I hate that.

So, today, we scale back quite a bit from 'yesterday's' problem.

Today we take on part of the problem.

end-rant.

Today we scale back quite a bit. Transactions in the block-chain are in
blocks. Now-a-days, seeing the popularity of BitCoin, there are oftentimes
hundreds of transactions in a single block.

So, we have sample data, right at our fingertips, using the functions defined
in the above imports.

From the latestBlock, extract the transactions of that block
--}

latestTransactions :: IO [Transaction]
latestTransactions =
   fmap (tx . head) (latestSummary >>= fetchBlocks . pure . blockHash)

{--
*Y2016.M09.D22.Solution> latestTransactions ~> xactions
*Y2016.M09.D22.Solution> length xactions ~> 634
--}

{--
Okay, in the block-chain, even though it's a P2P network, which makes one 
think that it's person-based, but that's not the case at all: bitcoin 
transactions are address-based, and the address isn't a physical addresss (that
is: 'anchored to a lat/long'-address), but the address of your 'wallet' from
when you do bitcoin transactions.

Makes sense.

You also see that Transaction has a set of addresses, but not directly, why?
Because each transaction may have multiple inputs and multiple outputs, each,
possibly, with an address.

What are all the addresses of a single transaction?
--}

addresses :: Transaction -> [Hash]
addresses = mapMaybe addr . (mapMaybe prevOut . inputs &&& out >>> uncurry (++))

{--
*Y2016.M09.D22.Solution> head xactions
TX {lockTime = 0, version = 1, size = 148,
    inputs = [In {sequence = 4294967295, prevOut = Nothing,
                  inScript = "035893060004ef55e357042c45433108d0c1a018c398555d0a425720537570706f727420384d200a666973686572206a696e78696e092f425720506f6f6c2f"}],
    time = 1474516463, txIndex = 176684638, vInSize = 1, vOutSize = 1,
    hashCode = "5b026c39da86a21df06f8295b2a714b3ada33cad2a8fd9482babcd7c53492ded",
    relayedBy = "123.56.129.45",
    out = [Out {spent = False, outTxIndex = 176684638, typ = 0,
                value = 1267140415, n = 0,
                addr = Just "1BQLNJtMDKmMZ4PyqVFfRuBNvoGhjigBKF",
                outScript = "76a914721afdf638d570285d02d3076d8be6a03ee0794d88ac"}]}
*Y2016.M09.D22.Solution> addresses it ~> ["1BQLNJtMDKmMZ4PyqVFfRuBNvoGhjigBKF"]

How many transactions are in the latestBlock?

*Y2016.M09.D22.Solution> length xactions ~> 634

How many (unique) addresses are in the latestBlock?

*Y2016.M09.D22.Solution> length . Set.fromList $ concatMap addresses xactions
2513

We have 2513 unique addresses in the latestBlock.

(You'll get different results when you run this, of course, because the
latestBlock value will be quite different for you.)
--}

-- I've imported the BitCoin-currency module as a foretaste of tomorrow's
-- problem of computing the number of bitcoins exchanged in a transaction.
