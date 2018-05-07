{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M05.D07.Exercise where

{--
Continuing on the daily-upload vein we started in Y2018.M05.D04...

Okay, we've downloaded a set of articles from the REST endpoint, so, now, we
need to get the context of what articles are already stored in the database so
we can do our triage in that context.

We already have a context declared: ArticleMetaData from ... centuries ago. Can
we use that to build our context here with the World Policy journal? Let's find
out!
--}

import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository.

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2018.M01.D29.Exercise (oneWeekAgo)
-- import Y2018.M01.D30.Exercise hiding (fetchArticleMetaData, fetchArticleMetadataStmt)  -- for ArticleMetaData
import Y2018.M05.D04.Exercise

-- download the article meta data up to from one week ago. How many metadata
-- did you accumulate?

{--
>>> conn <- connectInfo WPJ >>= connect
>>> wk <- oneWeekAgo conn
>>> wk
2018-04-15
>>> amd <- fetchArticleMetaData conn wk
*** Exception: SqlError {sqlState = "42703", sqlExecStatus = FatalError, 
      sqlErrorMsg = "column \"article_id\" does not exist", 
      sqlErrorDetail = "", sqlErrorHint = ""}
>>> close conn

So, okay, we have to rewrite the fetch statement for this database

Also, as the article id here integer (whereas in Pilot it's a string (uuid)),
so we need to generalize the AMD (ArticleMetaData type) 
--}

data ArticleMetaData a = AMD { artId :: a, published, lastUpdate :: Maybe Day }
   deriving (Eq, Show)

-- and now let's write the FromRow instance for this thing (recalling that it
-- will be an IxValue type, too

instance FromField a => FromRow (ArticleMetaData a) where
   fromRow = undefined

-- for the query

fetchArticleMetaDataStmt :: Query
fetchArticleMetaDataStmt =
   [sql|SELECT id,art_id,publish_dt,update_dt
        FROM article
        WHERE publish_dt > ?|]

-- it's kind of embarrassing that this statement is exactly the same except
-- that the column name is only slightly different, but eh, this happens in the
-- real world, so ... ... oh! also: improved declaration.

fetchArticleMetaData :: Connection -> Day -> IO [IxValue (ArticleMetaData Integer)]
fetchArticleMetaData conn day = undefined

-- How many AMDs from the WPJ did you download?
