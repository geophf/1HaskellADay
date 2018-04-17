{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D16.Solution where

{--
Today we're going to take a bunch of articles from various sources and upload
them to a single, simple data store, the structure of which is in the ER-diagram
at Y2018/M04/D16/archive-erd.png and described here in code.
--}

import Prelude hiding (log)

import qualified Codec.Compression.GZip as GZ

import Control.Monad ((>=>))

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toUpper)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git respository

import Data.AuditLogger
import Data.Logger
import Data.LookupTable
import Data.LookupTable
import Data.MemoizingTable
import Data.Time.Stamped

import Store.SQL.Connection
import Store.SQL.Util.AuditLogging
import Store.SQL.Util.Indexed hiding (ix)
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable (lookupTable)

-- 1. read each JSON from the directory

archiveDir, compressedDir, root :: FilePath
root = "Y2018/M04/D16/"
archiveDir = root ++ "articles/"
compressedDir = root ++ "compressed/"

-- the archives are

archives :: [FilePath]
archives = map (++ ".json")
   $ words ("bbcbusiness bbcentertainment bbcpolitics bbcsports bbctech "
            ++ "bleiap nyt_all wapo_all wsj_all")

data IxArt = IA { ix, text :: String }
   deriving (Eq, Show)

readArts :: (ByteString -> ByteString) -> (String -> String) 
         -> FilePath -> IO [IxArt]
readArts converter filer =
   fmap (map (uncurry IA) . Map.toList . fromJust . decodeMap . converter)
      . BL.readFile . filer

decodeMap :: ByteString -> Maybe (Map String String)
decodeMap = decode

{--
>>> arties <- readArts (archiveDir ++ "bbcbusiness.json")
>>> map ix arties
["0","1","10","2","3","4","5","6","7","8","9"]
--}

-- 2. Populate the lookup table for Publications

pubStmt :: Query
pubStmt = [sql|INSERT INTO publication_lk (publication) VALUES (?) returning id|]

class Tagged a where
   tag :: a -> String

data Publication = Pub String
   deriving (Eq, Show)

instance Tagged Publication where
   tag (Pub p) = p

instance ToRow Publication where
   toRow (Pub p) = [toField p]

pubs :: [Publication]
pubs = map Pub $ words "BBC BLE NYT WAP WSJ"

-- note that a pub is the first three letters from each file. Useful, that.

insertPubs :: Connection -> [Publication] -> IO [Index]
insertPubs conn = returning conn pubStmt

ixtagged :: Tagged t => [t] -> [Index] -> LookupTable
ixtagged pubs ixs = Map.fromList (zip (map tag pubs) (map idx ixs))

{--
>>> withConnection ARCHIVE (\conn -> insertPubs conn pubs >>= print . ixtagged pubs)
fromList [("BBC",1),("BLE",2),("NYT",3),("WAP",4),("WSJ",5)]
--}

-- 3. Do the same for files

fileStmt :: Query
fileStmt = [sql|INSERT INTO file_lk (file) VALUES (?) returning id|]

data SourceFile = Src String
   deriving (Eq, Show)

instance Tagged SourceFile where
   tag (Src s) = s

instance ToRow SourceFile where
   toRow (Src f) = [toField f]

-- (files are already defined)

insertSourceFiles :: Connection -> [SourceFile] -> IO [Index]
insertSourceFiles conn = returning conn fileStmt

-- a handy convert function:

file2pub :: FilePath -> Publication
file2pub = Pub . map toUpper . take 3

{--
>>> srcs = map Src archives
>>> withConnection ARCHIVE (flip insertSourceFiles srcs >=> print . ixtagged srcs)
{("bbcbusiness.json",1),("bbcentertainment.json",2),("bbcpolitics.json",3),
 ("bbcsports.json",4),("bbctech.json",5),("bleiap.json",6),("nyt_all.json",7),
 ("wapo_all.json",8),("wsj_all.json",9)}
--}

-- 4. insert the articles from each JSON file

data Article = Art { art :: IxArt, pub :: Publication, file :: SourceFile }
   deriving Show

data Article' = Art' IxArt Integer Integer

art2art' :: LookupTable -> LookupTable -> Article -> Article'
art2art' pubs files (Art art p f) =
   Art' art (pubs Map.! tag p) (files Map.! tag f)

instance ToRow Article' where
   toRow (Art' i p f) = [toField (ix i),toField p, toField f,toField (text i)]

artStmt :: Query
artStmt = [sql|INSERT INTO article (file_id,publication,file,body)
               VALUES (?,?,?,?)|]

insertArts :: Connection -> LookupTable -> LookupTable -> [Article] -> IO ()
insertArts conn pubs files =
   void . executeMany conn artStmt . map (art2art' pubs files)

{-- BONUS -----------------------------------------------------------------

Create an etl process that reads in each JSON and stores the articles of that
file (and publication). Include logging and auditing information.

--}

log :: String -> IO (Stamped LogEntry)
log = stampIt . Entry INFO "archive ETL" "Y2018.M04.D16.Solution"

etlArticleSet :: (FilePath -> IO [IxArt]) -> Connection -> LookupTable
              -> LookupTable -> LookupTable -> FilePath -> FilePath -> IO ()
etlArticleSet reader conn sev pubs files dir artFile =
   reader (dir ++ ('/':artFile))         >>= \ixarts ->
   log ("read in json from " ++ artFile) >>=
   insertStampedEntries conn sev . pure  >>
   insertArts conn pubs files (map (\art -> Art art (file2pub artFile) (Src artFile)) ixarts) >>
   log ("insert " ++ show (length ixarts) ++ " rows to article data table.") >>=
   insertStampedEntries conn sev . pure

-- Do that for each article; which includes creating the database connection,
-- reading in the lookup tables from the database, then, parse and store each
-- article JSON file

main'' :: (FilePath -> IO [IxArt]) -> [String] -> IO ()
main'' _ [] = mapM_ putStrLn ["", "etlArticleSet <dir> <file1> [file2 file3 ...]", "",
   "\tParses JSON <files> and uploads articles to ARCHIVE database", ""]
main'' readArts (dir:jsons) =
   putStrLn "etlArticleSet start..."                                >>
   withConnection ARCHIVE (\conn ->
      lookupTable conn "severity_lk"                      >>= \sev ->
      lookupTable conn "file_lk"                          >>= \files ->
      lookupTable conn "publication_lk"                   >>= \pubs ->
      mapM_ (etlArticleSet readArts conn sev pubs files dir) jsons >>
      lookupTable conn "active_lk"                        >>= \actv ->
      lookupTable conn "action_lk"                        >>= \actn ->
      storeAuditInfo conn actv actn "n/a" (IxV 0 (last jsons)))     >>
   putStrLn "... done."

{-- For the plain JSON files the solution is:
main' :: [String] -> IO ()
main' = main'' (readArts id id)
--}

{-- BONUS-BONUS -----------------------------------------------------------

Do all the above, but this time, each file is in compressed/ ... use the
GZip codec to decompress each JSON article archive
--}

-- for the compressed JSON archive, the answer is:

main' :: [String] -> IO ()
main' = main'' (readArts GZ.decompress (++ ".gz"))

{--
>>> main' ("arts/ArticleJsons/":delete "nyt_all.json" archives)
etlArticleSet start...
... done.

>>> main' ["arts/ArticleJsons/","nyt_all.json"]
etlArticleSet start...
... done.

... the (full) NYT article set took a while, but BOOM! There we go, a new
database of articles from various sources.
--}
