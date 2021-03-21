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

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

extractJSONQuery :: Query
extractJSONQuery =
   "SELECT source_id, file FROM SOURCE WHERE processed=? AND source_type_id=?"

data JSONFile = JSONFile { fileId :: Integer, file :: String }
   deriving (Eq, Show)

instance FromRow JSONFile where
   fromRow = JSONFile <$> field <*> field

extractJSON :: FromJSON a => String -> Connection -> LookupTable
                          -> IO [IxValue a]
extractJSON lk conn srcs =
   query conn extractJSONQuery (False, srcs Map.! lk)             >>=
   return . mapMaybe (\(JSONFile i f) -> IxV i <$> decode (BL.pack f))

extractRanks :: Connection -> LookupTable -> IO [IxValue MetaData]
extractRanks = extractJSON "RANKING"

data Listings = Listings { unlist :: [Listing] }

instance Show Listings where
   show (Listings ls) =
      "Listings (" ++ show (length ls) ++ "), e.g.: " ++ show (take 3 ls)

instance FromJSON Listings where
   parseJSON = withObject "data" $ \v -> Listings <$> v .: "data"

extractListings :: Connection -> LookupTable -> IO [IxValue Listings]
extractListings = extractJSON "LISTING"

{--
>>> withConnection ECOIN (\conn -> lookupTable conn "source_type_lk >>=
                                   extractRanks conn                >>=
                                   mapM_ (\(IxV i (MetaData s _)) -> print (i,s)))
(38,Status 2021-03-19 0 Nothing 16 1 Nothing)

... returns only the non-processed rank-files.

Okay, we're extracting and translating the JSON. Now we have to load these
data into the tables... somehow.

Also, note the dates. We have to correct them to local dates before we upload
data twice for one day.

TODO!
--}
