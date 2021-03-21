{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types.Quote where

import Data.Aeson
import Data.Aeson.Types

data Quote = Quote {
   price :: Double,
   volumn24h        :: Double,
   percentChange1h  :: Maybe Double,
   percentChange24h :: Maybe Double,
   percentChange7d  :: Maybe Double,
   percentChange30d :: Maybe Double,
   percentChange60d :: Maybe Double,
   percentChange90d :: Maybe Double }
      deriving (Eq, Ord, Show) 

instance FromJSON Quote where
   parseJSON = withObject "quote" $ \v ->
      Quote <$> v .: "price"
            <*> v .: "volume_24h"
            <*> v .:?  "percent_change_1h"
            <*> v .:?  "percent_change_24h"
            <*> v .:?  "percent_change_7d"
            <*> v .:?  "percent_change_30d"
            <*> v .:?  "percent_change_60d"
            <*> v .:?  "percent_change_90d"
