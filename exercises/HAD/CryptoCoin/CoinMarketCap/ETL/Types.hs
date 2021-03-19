{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.Types where

-- types shared across the ETL codebase.

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.LookupTable

import CryptoCoin.CoinMarketCap.Types

import Store.SQL.Util.Indexed

extractRanksQuery :: Query
extractRanksQuery =
   "SELECT source_id, file FROM SOURCE WHERE processed=? AND source_type_id=?"

data RankFile = RankFile { fileId :: Integer, file :: String }
   deriving (Eq, Show)

instance FromRow RankFile where
   fromRow = RankFile <$> field <*> field

rankingIdx :: LookupTable -> Integer
rankingIdx srcs = srcs Map.! "RANKING"

extractRanks :: Connection -> LookupTable -> IO [IxValue MetaData]
extractRanks conn srcs =
   query conn extractRanksQuery (False, rankingIdx srcs) >>=
   return . mapMaybe (\(RankFile i f) -> IxV i <$> decode (BL.pack f))

{--
>>> withConnection ECOIN (\conn -> extractRanks conn >>=
                                   mapM_ (\(IxV i (MetaData s _)) -> print (i,s)))
(1, Status 2021-03-09 0 Nothing 20 1 Nothing)
(2, Status 2021-03-08 0 Nothing 24 1 Nothing)
(3, Status 2021-03-08 0 Nothing 19 1 Nothing)
...
(9, Status 2021-02-25 0 Nothing 22 1 Nothing)
(10, Status 2021-02-24 0 Nothing 15 1 Nothing)
(11, Status 2021-02-24 0 Nothing 27 1 Nothing)
(12, Status 2021-02-22 0 Nothing 24 1 Nothing)

Okay, we're extracting and translating the JSON. Now we have to load these
data into the tables... somehow.

Also, note the dates. We have to correct them to local dates before we upload
data twice for one day.

TODO!
--}
