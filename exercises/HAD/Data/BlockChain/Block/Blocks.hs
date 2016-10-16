{-# LANGUAGE OverloadedStrings #-}

module Data.BlockChain.Block.Blocks where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Maybe (fromJust)
import Network.HTTP.Conduit

-- availabe from the @1HaskellADay git repository

import qualified Data.BlockChain.Block.Summary as Smy
import Data.BlockChain.Block.Transactions (Transaction)
import qualified Data.BlockChain.Block.Transactions as Txn
import Data.BlockChain.Block.Types
import Data.Tree.Merkle

{--
There is a reason the summary is called the summary, for, when we download the
entire block we get:

-rw-r--r--  1 geophf  staff  4020041 Sep  6 00:55 lateblk.json
-rw-r--r--  1 geophf  staff    12763 Sep  6 00:40 latesum.json

or, put another way, the latest block summary is 12k, but the entire block,
with all its transactions is 4M!

Eep!

Let's just deal with blocks over the wire for now, instead of putting 4 meg
into this git repository with one push.

From the above Summary import, we know how to load the latest block summary.
Do that, get the hash for the block, and from the hash, download the entire
block into memory and extract the list of transactions.
--}

data Block = Block { blockhash :: Hash, ver :: Integer, prevBlock :: String,
                     merkleRoot :: String, time, bits, fee, nonce :: Integer,
                     nTx, size :: Int, blockIdx :: Integer, mainChain :: Bool,
                     height, receivedTime :: Integer, relayedBy :: String,
                     tx :: [Transaction] }
   deriving (Eq, Ord, Show)

instance Sheesh Block where
   hash = blockhash

instance FromJSON Block where
   parseJSON (Object o) = Block <$> o .: "hash"        <*> o .: "ver"
                                <*> o .: "prev_block"  <*> o .: "mrkl_root"
                                <*> o .: "time"        <*> o .: "bits"
                                <*> o .: "fee"         <*> o .: "nonce"
                                <*> o .: "n_tx"        <*> o .: "size"
                                <*> o .: "block_index" <*> o .: "main_chain"
                                <*> o .: "height"      <*> o .: "received_time"
                                <*> o .: "relayed_by"  <*> o .: "tx"

rawBlockURL :: FilePath
rawBlockURL = "https://blockchain.info/rawblock/"

readBlock :: FilePath -> Hash -> IO Block
readBlock url hash = fmap (fromJust . decode) $ simpleHttp (url ++ hash)

-- hint look at above Summary import on how to read in the latest summary, then
-- look at how to extract the block-hash from the summary report.

{--
*Y2016.M09.D06.Solution> Smy.latestSummary ~> smmy
*Y2016.M09.D06.Solution> readBlock rawBlockURL (Smy.blockHash smmy)

How many transactions are there in this block? What is the average size of
the transactions?

For block (hash: "0000000000000000039eb091b53b88042b9dc578285604914f6837c470c075df"):

*Y2016.M09.D06.Solution> length (tx blk) ~> 191
*Y2016.M09.D06.Solution> Txn.transactionsMeanSize (tx blk) ~> 5211

We should be able to link the transcation in this full block with the
summary block transaction index ... we'll get to doing that another day.
--}

-- From a set of hashes we want to fetch the blocks:

fetchBlocks :: [Hash] -> IO [Block]
fetchBlocks = mapM (readBlock rawBlockURL)

-- Now we want a specific kind of Merkle leaf. As we already have the hash,
-- we use the block hash for the leaf hash

mkBlockLeaf :: Block -> Leaf Block
mkBlockLeaf = uncurry Leaf . (blockhash &&& id)
