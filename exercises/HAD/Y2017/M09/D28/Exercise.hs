{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D28.Exercise where

{--
Yesterday, and the two days prior, we focused on ETL for names, a tricky subject
that deserve 3 (or even more) days of exercises.

Today, we're going back to the reified article concept from the days prior
and store those articles into the database. So, we're storing articles with
metadata, raw names, and then parsing the names.

Looks like we've got an ETL for articles on our hands!
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts (inserter)

import Y2017.M09.D25.Exercise
import Y2017.M09.D26.Exercise (extractArticles)

-- Given the following insert statement:

insertArtsStmt :: Query
insertArtsStmt = [sql|INSERT INTO article (src_id,title,author,publish_dt,abstract,
                                           full_text,people,locations)
                      VALUES (?,?,?,?,?,?,?) returning id|]

-- create a ToRow instance of the Article type:

instance ToRow Article where
   toRow art = undefined

-- Now, extract the articles from the compressed archive (extractArticles),
-- and insert the articles into the database:

insertArts :: Connection -> [Article] -> IO [Index]
insertArts conn arts = undefined

-- Then say: YAY! and throw confetti!
