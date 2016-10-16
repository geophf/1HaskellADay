{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Data.BlockChain.Block.Transactions where

import Control.Arrow ((&&&), (>>>))
import Control.Monad ((>=>))
import Crypto.Hash
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Network.Socket

-- below import available on 1HaskellADay git repository

import Data.BlockChain.Block.Types
import Data.BlockChain.Block.Utils

{--
Last Friday we looked at reading in a summary of a Block of the block chain.
Part of that summary data was a set of transaction indices (called 'tx' in the
JSON). But what are these transactions?

Today we will take a sample transaction, represent it in Haskell, and read in
that transaction-as-JSON.
--}

data Packet = Pck [Transaction] deriving Show

instance FromJSON Packet where
   parseJSON (Object o) = Pck <$> o .: "tx"

data Transaction =
   TX { lockTime, version, size :: Integer, inputs :: [Input],
        time, txIndex, vInSize, vOutSize :: Integer, hashCode :: Hash,
        relayedBy :: HostName, out :: [Output] }
      deriving (Eq, Ord, Show)

instance Sheesh Transaction where hash = hashCode

instance FromJSON Transaction where
   parseJSON (Object o) = TX <$> o .: "lock_time"  <*> o .: "ver"
                             <*> o .: "size"       <*> o .: "inputs"
                             <*> o .: "time"       <*> o .: "tx_index"
                             <*> o .: "vin_sz"     <*> o .: "vout_sz"
                             <*> o .: "hash"       <*> o .: "relayed_by"
                             <*> o .: "out"

-- of course, to read in a SHA256 digest as JSON, we need to define that

instance FromJSON (Digest SHA256) where
   parseJSON (String s) =
     return . fromJust . digestFromByteString . B.pack $ T.unpack s

data Input =
   In { sequence :: Integer, prevOut :: Maybe Output, inScript :: String }
      deriving (Eq, Ord, Show)

instance FromJSON Input where
   parseJSON (Object o) = In <$> o .: "sequence" <*> o .:? "prev_out"
                             <*> o .: "script"

data Output =
   Out { spent :: Bool, outTxIndex, typ, value, n :: Integer,
         addr :: Maybe String, outScript :: String }
      deriving (Eq, Ord, Show)

instance FromJSON Output where
   parseJSON (Object o) = Out <$> o .:  "spent" <*> o .: "tx_index"
                              <*> o .:  "type"  <*> o .: "value" <*> o .: "n"
                              <*> o .:? "addr"  <*> o .: "script"

-- With the above types having FromJSON instances defined, read in the two
-- transactions at this directory or also at the url:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M09/D05/txs.json
-- (ignoring, for now, the other paired-values outside the tx-json-list)

readTransactions :: FilePath -> IO [Transaction]
readTransactions = BL.readFile >=> (\(Just (Pck txns)) -> return txns) . decode

{--
*Y2016.M09.D05.Solution BL> BL.readFile "Y2016/M09/D05/txs.json" ~> json
*Y2016.M09.D05.Solution BL> let mbpcks = (decode json) :: Maybe Packet
*Y2016.M09.D05.Solution BL> let (Pck txns) = fromJust mbpcks 
--}

-- what is the average size of the (albeit very small) sample set of transcactions?

transactionsMeanSize :: [Transaction] -> Int
transactionsMeanSize = sum . map (fromIntegral . size) &&& length >>> uncurry div

-- *Y2016.M09.D05.Solution> transactionsMeanSize txns ~> 201
