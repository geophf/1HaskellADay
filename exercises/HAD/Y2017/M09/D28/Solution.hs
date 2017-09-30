{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D28.Solution where

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
import Store.SQL.Util.Inserts (look, byt, byteStr, inserter)

import Y2017.M09.D22.Exercise (dir, arts)
import Y2017.M09.D25.Solution
import Y2017.M09.D26.Solution (extractArticles)

insertArtsStmt :: Query
insertArtsStmt = [sql|INSERT INTO article (src_id,title,author,publish_dt,url,
                                           abstract,full_text,people,locations)
                      VALUES (?,?,?,?,?,?,?,?,?) returning id|]

-- create a ToRow instance of the Article type:

instance ToRow Article where
   toRow art = [toField (srcId art),toField (title art), toField (author art),
                lookm "Publication date" art, toField (url art),
                byt abstract art, byt fullText art, lookm "People" art,
                lookm "Location" art]
      where lookm r = look r metadata

-- Now, extract the articles from the compressed archive (extractArticles),
-- and insert the articles into the database:

insertArts :: Connection -> [Article] -> IO [Index]
insertArts = insertRows insertArtsStmt

-- Then say: YAY! and throw confetti!

{--
>>> articles <- extractArticles <$> BL.readFile (dir ++ arts)
>>> connectInfo 
ConnectInfo {connectHost = "...",...}
>>> conn <- connect it
>>> insertArts conn articles
>>> close conn

$ SELECT count(1) FROM article;
11

YAY!

$ SELECT abstract FROM article WHERE id in (SELECT max(id) FROM article);

In a poem called "Three Modes of History and Culture," from 1969, a kind of 
updated Muddy Waters blues, ...

YAY! THROWS CONFETTI!
--}
