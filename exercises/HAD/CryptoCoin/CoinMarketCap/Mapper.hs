{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Mapper where

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Graph

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher

ccdir :: FilePath
ccdir = "CryptoCoin/CoinMarketCap/"

ccmapJSON :: FilePath
ccmapJSON = ccdir ++ "coinmarketcap-map.json"

data MetaData = MD Status [Coin']
   deriving (Eq, Ord, Show)

instance FromJSON MetaData where
   parseJSON = withObject "Metadata" $ \v ->
      MD <$> v .: "status" <*> v .: "data"

data Status = Status String Int (Maybe String) Int Int (Maybe String)
   deriving (Eq, Ord, Show)

instance FromJSON Status where
   parseJSON = withObject "Status" $ \v ->
      Status <$> v .: "timestamp" <*> v .: "error_code"
             <*> v .:? "error_message" <*> v .: "elapsed"
             <*> v .: "credit_count" <*> v .:? "notice"

data Coin' = Coin' { id :: Idx, name, symbol, slug :: String,
                     rank, is_active :: Int,
                     first_historical_data, last_historical_data :: String,
                     platform :: Maybe CoinRef' }
   deriving (Eq, Ord, Show)

instance FromJSON Coin' where
   parseJSON = withObject "Raw coin" $ \v ->
      Coin' <$> v .: "id" <*> v .: "name" <*> v .: "symbol" <*> v .: "slug"
            <*> v .: "rank" <*> v .: "is_active"
            <*> v .: "first_historical_data" <*> v .: "last_historical_data"
            <*> v .:? "platform"

type TokenAddress = String

data CoinRef' = CR' Idx TokenAddress           -- indicating Token?
   deriving (Eq, Ord, Show)

instance Indexed CoinRef' where
   idx (CR' i _) = i

instance FromJSON CoinRef' where
   parseJSON = withObject "ref" $ \v ->
      CR' <$> v .: "id" <*> v .: "token_address"

fetchMap :: FilePath -> IO (Maybe MetaData)
fetchMap file = decode <$> BL.readFile file

{--
>>> fetchMap ccmapJSON
...
>>> let (Just (MD stats coins)) = it
>>> stats
Status "2021-02-22T19:57:10.205Z" 0 Nothing 24 1 Nothing

>>> take 3 coins 
[Coin' {id = 1, name = "Bitcoin", symbol = "BTC", slug = "bitcoin", rank = 1, 
        is_active = 1, first_historical_data = "2013-04-28T18:47:21.000Z", 
        last_historical_data = "2021-02-22T19:54:37.000Z", platform = Nothing},
 Coin' {id = 2, name = "Litecoin", symbol = "LTC", slug = "litecoin", rank = 8, 
        is_active = 1, first_historical_data = "2013-04-28T18:47:22.000Z", 
        last_historical_data = "2021-02-22T19:54:03.000Z", platform = Nothing},
 Coin' {id = 3, name = "Namecoin", symbol = "NMC", slug = "namecoin", 
        rank = 541, is_active = 1, 
        first_historical_data = "2013-04-28T18:47:22.000Z", 
        last_historical_data = "2021-02-22T19:54:02.000Z", platform = Nothing}]
>>> take 3 $ mapMaybe platform coins
[CR' 1839 "0xba2ae424d960c26247dd6c32edc70b295c744c43",
 CR' 1027 "0x2e98a6804e4b6c832ed0ca876a943abd3400b224",
 CR' 1027 "0x63f88a2298a5c4aee3c216aa6d926b184a4b2437"]
>>> length coins
4132

>>> length $ filter ((== 1) . is_active) coins
4132

>>> length $ filter ((/= Nothing) . platform ) coins
2870

Okay, so we have tokens and we have coins, and we have ranking of both,
and the ranking may change daily (?) ... let's graph these data.
--}

data Duration = Duration { first, last :: Date }
   deriving (Eq, Ord, Show)

data CoinInfo = CoinInfo Idx Name Symbol String Bool
   deriving (Eq, Ord, Show)

class CoinData a where
   info :: a -> CoinInfo

data Coin = Coin CoinInfo
   deriving (Eq, Ord, Show)

data Token = Token { coininf :: CoinInfo, coinRef :: Idx, token :: String }
   deriving (Eq, Ord, Show)
