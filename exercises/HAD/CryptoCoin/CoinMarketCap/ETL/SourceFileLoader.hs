{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.SourceFileLoader where

import qualified Data.ByteString.Char8 as BL

import Data.Char (toLower)

import Data.List (isSuffixOf)
import qualified Data.Map as Map

import Data.Time

import System.Directory
import System.Environment (getEnv)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Control.Presentation
import Control.Scan.CSV

import Data.LookupTable
import Data.Time.TimeSeries (today)

import Store.SQL.Util.LookupTable

import Store.SQL.Connection

uploadFile :: Integer -> FilePath -> Connection -> IO (FilePath, Integer)
uploadFile sourceType filename conn =
   readFile filename                        >>= \file ->
   today                                    >>= \tday ->
   query conn
         "INSERT INTO source (source_type_id, file_name, for_day, file) VALUES (?, ?, ?, ?) RETURNING source_id"
         (sourceType, filename, tday, file) >>= \[Only i] ->
   putStrLn ("Uploaded " ++ filename)       >>
   removeFile filename                      >>
   putStrLn ("Removed file " ++ filename)   >>
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

go :: IO ()
go = withConnection ECOIN (\conn ->
   lookupTable conn "source_type_lk" >>= \src ->
   getEnv "COIN_MARKET_CAP_DIR"      >>= \cmcDir ->
   let uploader dir typ = uploadAllFilesAt (cmcDir ++ ('/':dir ++ "/2021"))
                                           (src Map.! typ) conn
   in  uploader "rankings" "RANKING" >> 
       uploader "listings" "LISTING" >>
       uploader "scores"   "FCAS"    >>
       sources conn)

{--
>>> go
Uploaded coins-2021-03-09.json
Uploaded listings-2021-03-09.json

SQL query to check that the database is populated:
--}

data Source = Source { idx :: Integer, source :: String, forDay :: Day,
                       file :: String, processed :: Bool }
   deriving (Eq, Show)

instance FromRow Source where
   fromRow = Source <$> field <*> field <*> field <*> field <*> field

instance Univ Source where
   explode (Source i s d f p) = [show i, s, show d, f, showBool p]

showBool :: Bool -> String
showBool = (flip (:) . tail <*> toLower . head) . show

sourceQuery :: Day -> Query
sourceQuery tday = Query . BL.pack $ unlines [
   "SELECT a.source_id, b.source_type, a.for_day, a.file_name, a.processed",
   "FROM source a",
   "INNER JOIN source_type_lk b ON b.source_type_id=a.source_type_id",
   concat ["WHERE a.for_day > '", show tday, "'"],
   "ORDER BY a.file_name DESC"]

srcs :: Connection -> Day -> IO [Source]
srcs conn day = 
   let srcQuery = sourceQuery day in
   BL.putStrLn (fromQuery srcQuery) >> query_ conn srcQuery

sources :: Connection -> IO ()
sources conn = getCurrentTime                     >>=
               srcs conn . addDays (-5) . utctDay >>=
               csvHeader                          >>=
               mapM_ (putStrLn . uncsv)
  where csvHeader s = putStrLn "id,source_type,for_day,file_name,processed" >>
                      return s
