{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeSynonymInstances #-}

module Y2017.M12.D26.Exercise where

{--
Yesterday, we were able to parse the HTML content of articles stored in JSON.

But we want to take a step back today, and go back to before then when we
simply ingested the JSON as blocks of (unprocessed articles). What we're going
to do today, instead of parsing these blocks (as we started yesterday), is
to store what we've scanned.
--}

-- below import available via 1HaskellADay git repository

import Y2017.M12.D20.Exercise

{--
Read in subset.json from D20 exercise, now: save each article block to the
PostgreSQL database.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow

import Store.SQL.Util.Indexed

instance ToRow Article where
   toRow art = undefined

-- The insert statement gives the Article structure
-- (also image attached from the Entity-relation diagram)

insertArticleStgStmt :: Query
insertArticleStgStmt =
    [sql|INSERT INTO article_stg (block) VALUE (?) returning id|]

insertStagedArt :: Connection -> [Article] -> IO [Index]
insertStagedArt conn arts = undefined

-- insertStagedArt stores a set of articles and returns the ids the database
-- assigns to these articles it stores

-- What are these stored article ids? How many articles did you store?
