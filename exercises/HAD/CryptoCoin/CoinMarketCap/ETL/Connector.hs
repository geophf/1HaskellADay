{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.Connector where

import System.Environment
import Database.PostgreSQL.Simple

connection :: IO ConnectInfo
connection =
   getEnv "SQL_DAAS_USERNAME" >>= \user ->
   getEnv "SQL_DAAS_PASSWORD" >>= \pass ->
   getEnv "SQL_DAAS_SERVER_PORT" >>= \port ->
   getEnv "SQL_DAAS_SERVER_URL_ECOIN" >>= \url ->
   getEnv "SQL_DAAS_DB_NAME_ECOIN" >>= \db ->
   return defaultConnectInfo {
      connectHost     = url,
      connectDatabase = db,
      connectUser     = user,
      connectPort     = read port,
      connectPassword = pass }
