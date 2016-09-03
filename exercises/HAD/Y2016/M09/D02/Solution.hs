{-# LANGUAGE OverloadedStrings #-}

module Y2016.M09.D02.Solution where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
-- import qualified Data.Text as T
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
   parseJSON (Object o) =
      Block <$> o .: "hash"   <*> o .: "time"      <*> o .: "block_index"
            <*> o .: "height" <*> o .: "txIndexes"

readBlock :: FilePath -> IO Block
readBlock = fmap parseBlock . BL.readFile

parseBlock :: ByteString -> Block
parseBlock = fromJust . decode

-- The block is available at this directory or at the URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M09/D02/someblock.json

{--
*Y2016.M09.D02.Solution> readBlock "Y2016/M09/D02/someblock.json" ~> tehBlock
Block {blockHash = "0000...44c5...", time = 1472781602, blockIndex = 1140658, 
       height = 427888, txIndices = [172292984,172291712,...]}
--}

-- With the read-in block, answer me this: how many transaction indices are in
-- this block?

txIdxCount :: Block -> Int
txIdxCount = length . txIndices

-- *Y2016.M09.D02.Solution> length $ txIndices it ~> 2646 -- whew!

{-- BONUS -----------------------------------------------------------------

So, you've read a block from file. Great! Now, read the latest block securely 
from blockchain.info. The call-URL is: https://blockchain.info/latestblock
--}

latestBlockURL :: String
latestBlockURL = "https://blockchain.info/latestblock"

latestBlock :: IO Block
latestBlock = fmap parseBlock (simpleHttp latestBlockURL)

-- what is the hash Id of the latest block you read? How many transactions does
-- it contain?

-- *Y2016.M09.D02.Solution> latestBlock ~> bl
-- Block {blockHash = "0000...12cf...", time = 1472870698, blockIndex = 1142371,
--        height = 428043, txIndices = [172525204,172524853,...]}

-- *Y2016.M09.D02.Solution> txIdxCount bl ~> 2081

-- WOOT!
