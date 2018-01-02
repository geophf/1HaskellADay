{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D27.Exercise where

{--
Yesterday, we stored the unparsed article set into the database. We also were 
able to parse the HTML content of articles stored in JSON the day prior.

Today, we continue parsing article information and then storing those parsed
data into our database.

From the article, we need to store in the database some stuff. These stuff are:

uuid
title
url
starttime
lastupdated
keywords
sections
authors
prologue
content

Now, DON'T PANIC! (tm)

We'll take parsing these fields one day at a time. The simple parsing fields
we'll do today (the transliteration from a simple field in JSON to a simple
field in Haskell), the more complex fields we'll handle a day at a time in turn.
--}

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.HTML.TagSoup

-- below import available via 1HaskellADay git repository

import Store.SQL.Util.Indexed (Index)

import Y2017.M12.D22.Exercise

{--
You see from the above imported exercise that we have already scanned and parsed
the uuid, the title, and the content of each article. We have three more tricky
parsing exercises: starttime and lastupdated for time, authors, which is already
parsed out a bit for us, and sections.

Let's flesh out our article structure from 'yesterday,' leaving the authors
and sections unparsed for now, but figure out how to ingest the times, then,
with the parsed information, save those articles to our PostgreSQL data store.
--}

import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- updated to take into account optionality and varying types in keywords
-- (see Y2017.M12.D28.Exercise)

data DatedArticle a =
   Carbon { uuid, title, url       :: String,
            prologue               :: Maybe String,
            authors                :: [a],
            starttime, lastupdated :: Maybe ZonedTime,
            sections               :: [String],
            keywords               :: [Value],
            content                :: [String],
            byline                 :: String }
      deriving Show

instance HTML (DatedArticle a) where
   body art = undefined

-- so, but how do we get from that wild and wonderful structure in the JSON
-- for dates to a Haskell Day value?

parseDate :: Value -> Parser Day
parseDate json = undefined

sampleDate :: ByteString
sampleDate = BL.unlines ["{",
                "\"rfc2822\": \"Tue, 12 Dec 2017 22:00:00 -0500\",",
                "\"utc\": \"1513134000000\",",
                "\"iso8601\": \"2017-12-12T22:00:00-05:00\"",
            "}"]

instance FromJSON a => FromJSON (DatedArticle a) where
   parseJSON (Object o) = undefined

-- Now, with that parsed structure, save the Article set to the database

instance ToField a => ToRow (DatedArticle a) where
   toRow art = undefined

-- The insert statement gives the Article structure
-- (also image attached from the Entity-relation diagram)

insertArticleStmt :: Query
insertArticleStmt =
   [sql|INSERT INTO article (src_id,update_dt,publish_dt,article_id,url,
                             abstract,full_text,rendered_text,sections,title,
                             authors)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id|]

-- please make sure abstract/prologue and full_text/content are HTML-tag-free.
-- hint: Y2017.M12.D22.Exercise

insertArts :: Connection -> [Index] -> [DatedArticle a] -> IO [Index]
insertArts conn srcIdx arts = undefined

-- from the source article ids in the article_stg table and the parsed articles,
-- store the articles in the database

{-- BONUS -----------------------------------------------------------------

Write an ETL process that reads in the JSON, stores the raw article information
in article_stg (hint: Y2017.M12.D26.Exercise), then stores the parsed article
information with the source article id in the database as well.
--}

main' :: [String] -> IO ()
main' jsonFilePath = undefined
