{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types.Internal where

import Data.Aeson

import Data.CryptoCurrency.Types   -- for indexed

data Coin' = Coin' { id :: Idx, name, symbol, slug :: String,
                     rank', is_active :: Int,
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

data CoinRef' = CR' Idx TokenAddress
   deriving (Eq, Ord, Show)

instance Indexed CoinRef' where
   idx (CR' i _) = i

instance FromJSON CoinRef' where
   parseJSON = withObject "ref" $ \v ->
      CR' <$> v .: "id" <*> v .: "token_address"

