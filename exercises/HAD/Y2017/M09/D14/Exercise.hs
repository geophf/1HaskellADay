{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D14.Exercise where

import qualified Codec.Compression.GZip as GZ
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Time
import Database.PostgreSQL.Simple
import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectionStringFromEnv)

{--
Today, we're going to upload the articles, their dates, and the article text
onto a postgre SQL database. You can do this on your own computer, if you have
that DBMS running, or you can upload to a DaaS (there are several, e.g.:
elephantSQL.com).

The first thing we need to do is to read in the articles and compress them. 
Well, we've done that:
--}

import Y2017.M09.D08.Exercise

{--
But instead of writing the compressed files out to the file-system, we'll
put them into a structure and (eventually) write those structures out to
the database.
--}

data Article = Art { artId :: String, published :: Day, text :: ByteString }

loadCompress :: URL -> Directory -> FileName -> IO Article
loadCompress url archive filename = undefined

-- Now, with that set of articles, we'll upload them to the database to
-- a table with the structure of Article:

insertArticles :: Connection -> [Article] -> IO ()
insertArticles conn arts = undefined
