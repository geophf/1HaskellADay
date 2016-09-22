module Y2016.M09.D22.Exercise where

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
latestTransactions = undefined

{--
N.b.: latestTransactions does not need an input URL value, as that is known
from blockchain.info

Hint: see Data.BlockChain.Transactions
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
addresses = undefined   -- recall that 'address' is hashed.

{--
So there are possibly many addresses in a transaction, and possibly many
transactions in the latest block.

How many transactions are in the latestBlock?
How many (unique) addresses are in the latestBlock?
--}

-- I've imported the BitCoin-currency module as a foretaste of tomorrow's
-- problem of computing the number of bitcoins exchanged in a transaction.
