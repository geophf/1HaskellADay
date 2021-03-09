{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.SourceFileLoader where

import Data.List (isSuffixOf)
import qualified Data.Map as Ma

import System.Directory
import System.Environment (getEnv)

import Database.PostgreSQL.Simple

import Data.LookupTable

import Store.SQL.Util.LookupTable

import CryptoCoin.CoinMarketCap.ETL.Connector (connection)

uploadFile :: Integer -> FilePath -> Connection -> IO (FilePath, Integer)
uploadFile sourceType filename conn =
   readFile filename >>= \file ->
   query conn
         "INSERT INTO source (source_type_id, file_name, file) VALUES (?, ?, ?) RETURNING source_id"
         (sourceType, filename, file) >>= \[Only i] ->
   putStrLn ("Uploaded " ++ filename) >>
   return (filename, i)

uploadAllFilesAt :: FilePath -> Integer -> Connection -> IO [(FilePath, Integer)]
uploadAllFilesAt dir srcTyp conn =
   listDirectory dir >>= \files ->
   setCurrentDirectory dir >>
   mapM (flip (uploadFile srcTyp) conn) (filter (".json" `isSuffixOf`) files)

{--
>>> conn <- connection >>= connect
>>> src <- lookupTable conn "source_type_lk"
>>> src
fromList [("FCAS",3),("LISTING",2),("RANKING",1)]
>>> cmcDir <- getEnv "COIN_MARKET_CAP_DIR"
>>> uploadAllFilesAt (cmcDir ++ "/rankings/2021") (src Map.! "RANKING") conn
Uploaded coins-2021-03-01.json
Uploaded coins-2021-03-06.json
Uploaded coins-2021-02-27.json
Uploaded coins-2021-02-26.json
Uploaded coins-2021-03-07.json
Uploaded coins-2021-03-04.json
Uploaded coins-2021-03-08.json
Uploaded coins-2021-02-25.json
Uploaded coins-2021-02-24.json
Uploaded coins-2021-03-05.json
Uploaded coins-2021-02-28.json
Uploaded coins-2021-02-23.json
Uploaded coins-2021-03-02.json
Uploaded coins-2021-03-03.json
Uploaded coins-2021-02-22.json
[("coins-2021-03-01.json",1),("coins-2021-03-06.json",2),
 ("coins-2021-02-27.json",3),("coins-2021-02-26.json",4),
 ("coins-2021-03-07.json",5),("coins-2021-03-04.json",6),
 ("coins-2021-03-08.json",7),("coins-2021-02-25.json",8),
 ("coins-2021-02-24.json",9),("coins-2021-03-05.json",10),
 ("coins-2021-02-28.json",11),("coins-2021-02-23.json",12),
 ("coins-2021-03-02.json",13),("coins-2021-03-03.json",14),
 ("coins-2021-02-22.json",15)]

>>> uploadAllFilesAt (cmcDir ++ "/listings/2021") (src Map.! "LISTING") conn
Uploaded listings-2021-03-05.json
[("listings-2021-03-05.json",16)]

>>> close conn
--}
