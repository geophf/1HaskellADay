{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types.Internal where

import Data.Aeson

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)

import System.Environment (getEnv)

import Data.CryptoCurrency.Types   -- for indexed
import CryptoCoin.CoinMarketCap.Types.Quote

data Coin' = Coin' { id :: Idx, name, symbol, slug :: String,
                     rank', is_active :: Int,
                     first_historical_data, last_historical_data :: String,
                     platform :: Maybe CoinRef' }
   deriving (Eq, Ord, Show)

instance FromJSON Coin' where
   parseJSON = withObject "Raw coin" $ \v ->
      Coin' <$> v .: "id"
            <*> v .: "name"
            <*> v .: "symbol"
            <*> v .: "slug"
            <*> v .: "rank"
            <*> v .: "is_active"
            <*> v .: "first_historical_data"
            <*> v .: "last_historical_data"
            <*> v .:? "platform"

type TokenAddress = String

data CoinRef' = CR' Idx TokenAddress
   deriving (Eq, Ord, Show)

instance Indexed CoinRef' where
   idx (CR' i _) = i

instance FromJSON CoinRef' where
   parseJSON = withObject "ref" $ \v ->
      CR' <$> v .: "id" <*> v .: "token_address"

data Listing' = Listing' Integer Integer Double Double (Maybe Double) [String] (Map String Quote)
   deriving Show

instance FromJSON Listing' where
   parseJSON = withObject "listing" $ \v ->
      Listing' <$> v .: "id" 
               <*> v .: "num_market_pairs" 
               <*> v .: "circulating_supply"
               <*> v .: "total_supply"
               <*> v .:? "max_supply"
               <*> v .: "tags"
               <*> v .: "quote"

sample :: String -> IO ByteString
sample thing =
   getEnv "COIN_MARKET_CAP_DIR" >>=
   BL.readFile . (++ "/ETL/sample" ++ thing ++ ".json")
