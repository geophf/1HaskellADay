module Y2016.M09.D23.Exercise where

-- below imports available from 1HaskellADay git repository

import Data.BlockChain.Block.Blocks
import Data.BlockChain.Block.Summary
import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types

import Data.Monetary.BitCoin

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher

{--
Okay, we can get the addresses from the transactions, now let's get the
bitcoins from the transaction.

In each output of each transaction there is a field called 'value.' Let's
take a look at that:

*Y2016.M09.D23.Exercise> latestSummary
Summary {blockHash = "000000000000000002f56611a761f70493a1293dadec577cd04dbd7c9a4b4477", 
         time = 1474579944, blockIndex = 1148714, height = 431032, 
         txIndices = [176864461,176863406,176863432,176863441,176861799,...]}

Now if we go to blockchain.info and look up this blockHash, we see:

... Transactions ...

<hash>   2016-09-22 21:32:24
from-to hashes 13.02390887 BTC
        total: 13.02390887 BTC
<hash>   2016-09-22 21:26:33
from-to hashes  0.08284317 BTC
                0.07408666 BTC
        total:  0.15691983 BTC

... etc ...

Now, let's look at the values of the first two transactions of this block:

*Y2016.M09.D23.Exercise> mapM_ (print . value) (concatMap out (take 2 (tx (head blk))))
1302390887
8284317
7407666

HUH!  ... HUH! ... and again I say ... HUH!

Today's Haskell exercise. Distill the transaction to the inputs, and the
outputs with a BTC (BitCoin) value for each output. Use the transactions from
the latestSummary.
--}

data Trade = Trd { tradeHash :: Hash, executed :: String,
                   ins :: [Maybe Hash], outs :: [(Hash, BitCoin)] }
   deriving (Eq, Ord, Show)

-- What was the biggest trade, BitCoin-value-wise, from the latestSummary?
-- What was the biggest trade, most addresses (ins and outs)?

{-- BONUS -----------------------------------------------------------------

Represent Trade as a set of relations. Upload those relations to a graph
database for your viewing pleasure. Share an image of some of the transactions.

--}

trade2relations :: Trade -> [Relation a b c]
trade2relations = undefined

-- now: coming up with what types a, b, and c are will be fun ... for you.
