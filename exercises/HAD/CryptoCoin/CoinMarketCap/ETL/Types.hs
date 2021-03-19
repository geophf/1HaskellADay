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

extractRanksQuery :: Query
extractRanksQuery =
   "SELECT file FROM SOURCE WHERE processed=? AND source_type_id=?"

data RankFile = RankFile { file :: String }
   deriving (Eq, Show)

instance FromRow RankFile where
   fromRow = RankFile <$> field

rankingIdx :: LookupTable -> Integer
rankingIdx srcs = srcs Map.! "RANKING"

extractRanks :: Connection -> LookupTable -> IO [MetaData]
extractRanks conn srcs =
   query conn extractRanksQuery (False, rankingIdx srcs) >>=
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
