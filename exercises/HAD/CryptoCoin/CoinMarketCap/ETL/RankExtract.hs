{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.RankExtract where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import CryptoCoin.CoinMarketCap.Types

import Data.LookupTable

import Store.SQL.Util.LookupTable
import Store.SQL.Connection

extractRanksQuery :: Query
extractRanksQuery =
   "SELECT file FROM SOURCE WHERE processed=? AND source_type_id=?"

data JSONFile = JSON { file :: String }
   deriving (Eq, Show)

instance FromRow JSONFile where
   fromRow = JSON <$> field

extractRanks :: Connection -> IO [MetaData]
extractRanks conn =
   lookupTable conn "source_type_lk"                          >>= \srcs ->
   query conn extractRanksQuery (False, srcs Map.! "RANKING") >>=
   return . mapMaybe (decode . BL.pack . file)

{--
>>> withConnection ECOIN (\conn -> extractRanks conn >>=
                                   mapM_ (\(MetaData s _) -> print s))
Status 2021-03-09 0 Nothing 20 1 Nothing
Status 2021-03-08 0 Nothing 24 1 Nothing
Status 2021-03-08 0 Nothing 19 1 Nothing
...
Status 2021-02-25 0 Nothing 22 1 Nothing
Status 2021-02-24 0 Nothing 15 1 Nothing
Status 2021-02-24 0 Nothing 27 1 Nothing
Status 2021-02-22 0 Nothing 24 1 Nothing

Okay, we're extracting and translating the JSON. Now we have to load these
data into the tables... somehow.

Also, note the dates. We have to correct them to local dates before we upload
data twice for one day.

TODO!
--}

-- remember we need to set processed to true when we're all done here!
