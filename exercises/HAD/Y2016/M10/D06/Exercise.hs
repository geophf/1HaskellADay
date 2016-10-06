module Y2016.M10.D06.Exercise where

import Snap.Core -- if you want to use the Snap Framework 

import Data.BlockChain.Block.Blocks (Block, rawBlockURL, readBlock)
import Data.BlockChain.Block.Graphs
import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types

import Data.Tree.Merkle
import Data.XHTML

{--
So, we made a web service! YAY!

Okay, but it's a little clunky, so some housekeeping:

1. instead of latest block, have a GET-service that accepts a hash of a block
   and returns the transactions for that block.
--}

transactionsForBlock :: Hash -> IO [Transaction]
transactionsForBlock blockId = undefined

{--
2. And with that, show the block hash ID when you list all the hashes of the 
   transactions of that block
--}

-- so this would be a redefinition of yesterday's top function

top :: Block -> Snap ()
top blockinfo = undefined

{--
3. Showing the transaction as raw text is yucky. We have this construct called
   Trade that distills the Transaction information. Have a nice HTML-ish-like
   representation of a trade so my eyes don't cross when I scan a transcation.
--}

trade2HTML :: Trade -> Element
trade2HTML = undefined

{--
4. Now we need a Merkle tree-service.

4.a. For a block, give the hash of the root of the merkle tree of that block
     of transactions as a REST GET service
--}

rootHash :: MerkleTree a -> Hash
rootHash tree = undefined

{--
4.b. Given any hash of a Merkle node, give the left and right nodes' hashes
--}

childHashes :: MerkleTree a -> Hash -> (Hash, Hash)
childHashes tree hash = undefined

-- and to kick off the webservice:

main = undefined

-- hint: look at yesterday's exercise
