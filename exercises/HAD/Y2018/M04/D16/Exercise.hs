{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D16.Exercise where

{--
Today we're going to take a bunch of articles from various sources and upload
them to a single, simple data store, the structure of which is in the ER-diagram
at Y2018/M04/D16/archive-erd.png and described here in code.
--}

import Codec.Compression.GZip

import Data.Aeson

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git respository

import Data.AuditLogger
import Data.Logger
import Data.LookupTable
import Data.MemoizingTable

import Store.SQL.Connection
import Store.SQL.Util.AuditLogging
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging

-- 1. read each JSON from the directory

archiveDir :: FilePath
archiveDir = "Y2018/M04/D16/articles/"

-- the archives are

archives :: [FilePath]
archives = map (++ ".json")
   $ words ("bbcbusiness bbcentertainment bbcpolitics bbcsports bbctech "
            ++ "bleiap nyt_all wapo_all wsj_all")

data IxArt = IA { ix, text :: String }
   deriving (Eq, Show)

instance FromJSON IxArt where
   parseJSON (Object o) = undefined

readArts :: FilePath -> IO [IxArt]
readArts file = undefined

-- 2. Populate the lookup table for Publications

pubStmt :: Query
pubStmt = [sql|INSERT INTO publication_lk (publication) VALUES (?) returning id|]

class Tagged a where
   tag :: a -> String

data Publication = Pub String
   deriving (Eq, Show)

instance Tagged Publication where
   tag (Pub p) = undefined

instance ToRow Publication where
   toRow (Pub p) = undefined

pubs :: [Publication]
pubs = map Pub $ words "BBC BLE NYT WAP WSJ"

-- note that a pub is the first three letters from each file. Useful, that.

insertPubs :: Connection -> [Publication] -> IO [Index]
insertPubs conn pubs = undefined

ixtagged :: Tagged t => [t] -> [Index] -> LookupTable
ixtagged pubs ixs = undefined

-- 3. Do the same for files

fileStmt :: Query
fileStmt = [sql|INSERT INTO file_lk (file) VALUES (?) returning id|]

data SourceFile = Src String
   deriving (Eq, Show)

instance Tagged SourceFile where
   tag (Src s) = undefined

instance ToRow SourceFile where
   toRow (Src f) = undefined

-- (files are already defined)

insertSourceFiles :: Connection -> [SourceFile] -> IO [Index]
insertSourceFiles conn files = undefined

-- a handy convert function:

file2pub :: FilePath -> Publication
file2pub file = undefined

-- 4. insert the articles from each JSON file

data Article = Art { art :: IxArt, pub :: Publication, file :: SourceFile }
   deriving Show

data Article' = Art' IxArt Integer Integer

art2art' :: LookupTable -> LookupTable -> Article -> Article'
art2art' pubs files art = undefined

instance ToRow Article' where
   toRow art = undefined

artStmt :: Query
artStmt = [sql|INSERT INTO article (file_id,publication,file,body)
               VALUES (?,?,?,?)|]

insertArts :: Connection -> LookupTable -> LookupTable -> [Article] -> IO ()
insertArts conn pubs files arts = undefined

{-- BONUS -----------------------------------------------------------------

Create an etl process that reads in each JSON and stores the articles of that
file (and publication). Include logging and auditing information.

--}

etlArticleSet :: Connection -> LookupTable -> LookupTable -> FilePath -> IO ()
etlArticleSet conn pubs files artFile = undefined

-- Do that for each article; which includes creating the database connection,
-- reading in the lookup tables from the database, then, parse and store each
-- article JSON file

main' :: [String] -> IO ()
main' files = undefined

{-- BONUS-BONUS -----------------------------------------------------------

Do all the above, but this time, each file is in compressed/ ... use the
GZip codec to decompress each JSON article archive
--}
