{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.QuoteExtract where

{--

Okay, the listing format is:

https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyListingsLatest

We are going to assume we've populated the coin-table first, which we can
control by only calling this module from the CoinExtract module (which is
becoming ETL-main-ish.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow

import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.Types.Quote

