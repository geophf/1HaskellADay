module Y2016.M09.D05.Exercise where

import Crypto.Hash
import Data.Aeson
import Network.Socket

{--
Last Friday we looked at reading in a summary of a Block of the block chain.
Part of that summary data was a set of transaction indices (called 'tx' in the
JSON). But what are these transactions?

Today we will take a sample transaction, represent it in Haskell, and read in
that transaction-as-JSON.
--}

data Transaction =
   TX { lockTime, version, size :: Integer, inputs :: [Input],
        time, txIndex, vInSize, vOutSize :: Integer, hashCode :: String,
        relayedBy :: HostName, out :: [Output] }
      deriving (Eq, Ord, Show)

instance FromJSON Transaction where
   parseJSON = undefined

data Input =
   In { sequence :: Integer, prevOut :: Maybe Output, inScript :: String }
      deriving (Eq, Ord, Show)

instance FromJSON Input where
   parseJSON = undefined

data Output =
   Out { spent :: Bool, outTxIndex, typ, value, n :: Integer,
         addr, outScript :: String }
      deriving (Eq, Ord, Show)

instance FromJSON Output where
   parseJSON = undefined

-- With the above types having FromJSON instances defined, read in the two
-- transactions at this directory or also at the url:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M09/D05/txs.json
-- (ignoring, for now, the other paired-values outside the tx-json-list)

readTransactions :: FilePath -> IO [Transaction]
readTransactions = undefined

-- what is the average size of the (albeit very small) sample set of transcactions?

transactionsMeanSize :: [Transaction] -> Int
transactionsMeanSize = undefined
