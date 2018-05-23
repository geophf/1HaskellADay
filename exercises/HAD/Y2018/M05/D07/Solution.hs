{-# LANGUAGE FlexibleInstances, OverloadedStrings, QuasiQuotes #-}

module Y2018.M05.D07.Solution where

{--
Continuing on the daily-upload vein we started in Y2018.M05.D04...

Okay, we've downloaded a set of articles from the REST endpoint, so, now, we
need to get the context of what articles are already stored in the database so
we can do our triage in that context.

We already have a context declared: ArticleMetaData from ... centuries ago. Can
we use that to build our context here with the World Policy journal? Let's find
out!
--}

import qualified Data.ByteString.Char8 as BL

import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository.

import Control.List (singleton)
import Control.Logic.Frege ((<<-))

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Time

-- download the article meta data up to from one week ago. How many metadata
-- did you accumulate?

{--
>>> conn <- connectInfo WPJ >>= connect
>>> wk <- oneWeekAgo conn

or:

>>> wk = fromGregorian 2018 4 15
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

data ArticleMetaData a =
   AMD { artId :: a, published, lastUpdate :: Maybe Day }
      deriving (Eq, Show)

-- and now let's write the FromRow instance for this thing (recalling that it
-- will be an IxValue type, too

instance FromField a => FromRow (ArticleMD a) where
   fromRow = AMD' <$> field <*> field <*> (Just <$> field) <*> field

data ArticleMD a = AMD' Integer a (Maybe LocalTimestamp) (Maybe LocalTimestamp)
   deriving Show

amd2amd :: ArticleMD a -> IxValue (ArticleMetaData a)
amd2amd (AMD' ix id d u) = IxV ix (AMD id (d >>= d2d) (u >>= d2d))

-- for the query

fetchArticleMetaDataStmt :: Query
fetchArticleMetaDataStmt =
   [sql|SELECT id,art_id,publish_dt,update_dt
        FROM article
        WHERE publish_dt > ?|]

-- it's kind of embarrassing that this statement is exactly the same except
-- that the column name is only slightly different, but eh, this happens in the
-- real world, so ... ... oh! also: improved declaration.

fetchArticleMetaData :: Connection -> Day
                     -> IO [IxValue (ArticleMetaData Integer)]
fetchArticleMetaData = fmap (map amd2amd) <<- famd'

famd' :: Connection -> Day -> IO [ArticleMD Integer]
famd' conn = query conn fetchArticleMetaDataStmt . Only

-- How many AMDs from the WPJ did you download?

{--
>>> amd <- fetchArticleMetaData conn wk
>>> close conn
>>> length amd
5
>>> mapM_ print amd
IxV {ix = 1, val = AMD {artId = 22565, published = Just 2018-04-20, lastUpdate = Just 2018-04-20}}
IxV {ix = 2, val = AMD {artId = 22976, published = Just 2018-04-19, lastUpdate = Just 2018-04-20}}
IxV {ix = 3, val = AMD {artId = 22969, published = Just 2018-04-19, lastUpdate = Just 2018-04-20}}
IxV {ix = 4, val = AMD {artId = 22934, published = Just 2018-04-17, lastUpdate = Just 2018-04-17}}
IxV {ix = 5, val = AMD {artId = 22914, published = Just 2018-04-16, lastUpdate = Just 2018-04-19}}
--}

{--
Note on intermingling of paradigms.

It's a funny thing. When I import this module:

-- import Y2018.M01.D29.Solution (oneWeekAgo)

(with its superfluous dependency on Pilot structures, which I have since 
removed)

That has no lexical impact, the database connection fails.

When I do not import it, the database connection works.

The take-away here from me is to build modules small and not to import mega-
modules.
--}
