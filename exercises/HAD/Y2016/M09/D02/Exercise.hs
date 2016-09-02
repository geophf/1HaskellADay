module Y2016.M09.D02.Exercise where

import Data.Aeson
import Network.HTTP.Conduit

{--
Yesterday we started giving form to Merkle trees, but there's a lot of
developments to flesh out that structure, such as inserting nodes (so Merkle
trees constructed are well-balanced), and an efficient copy algorithm.

Those will come.

HOWEVER, the example data I used yesterday to store in the leaf nodes of the
Merkle tree were bitcoins, and that's just plain silly! If we are building a
block of the blockchain, we want representational data: transactions.

Where do I find such things?

Well: hello, internet! We have blocks available at:

https://blockchain.info/api/blockchain_api

WOOT!

So, let's explore (any given) block of the blockchain. First, what are blocks
and what is the blockchain?

https://en.bitcoin.it/wiki/Block
https://en.bitcoin.it/wiki/Block_chain

There you go, now you're all learnt up!

Okay, now, what is a particular block?

We will develop the RESTful interface to the block API as a bonus, but for now
we have a block saved locally here in JSON. So, with that block, read it in,
parse the JSON, and give the result as a Haskell Block value
--}

data Block = Block { blockHash :: String, time, blockIndex, height :: Integer, 
                     txIndices :: [Integer] }
   deriving (Eq, Ord, Show)

instance FromJSON Block where
   parseJSON = undefined

readBlock :: FilePath -> IO Block
readBlock = undefined

-- The block is available at this directory or at the URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M09/D02/someblock.json

-- With the read-in block, answer me this: how many transaction indices are in
-- this block?

txIdxCount :: Block -> Integer
txIdxCount = undefined

{-- BONUS -----------------------------------------------------------------

So, you've read a block from file. Great! Now, read the latest block securely 
from blockchain.info. The call-URL is: https://blockchain.info/latestblock
--}

latestBlock :: IO Block
latestBlock = undefined

-- what is the hash Id of the latest block you read? How many transactions does
-- it contain?
