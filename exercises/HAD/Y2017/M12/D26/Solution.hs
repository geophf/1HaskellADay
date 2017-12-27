{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeSynonymInstances #-}

module Y2017.M12.D26.Solution where

{--
Yesterday, we were able to parse the HTML content of articles stored in JSON.

But we want to take a step back today, and go back to before then when we
simply ingested the JSON as blocks of (unprocessed articles). What we're going
to do today, instead of parsing these blocks (as we started yesterday), is
to store what we've scanned.
--}

-- below import available via 1HaskellADay git repository

import Y2017.M12.D20.Solution

{--
Read in subset.json from D20 exercise, now: save each article block to the
PostgreSQL database.
--}

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField (toField)

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed

instance ToRow Block where
   toRow = pure . toField . BL.unpack . encodePretty

-- The insert statement gives the Article structure
-- (also image attached from the Entity-relation diagram)

insertArticleStgStmt :: Query
insertArticleStgStmt =
    [sql|INSERT INTO article_stg (block) VALUES (?) returning id|]

insertStagedArt :: Connection -> [Block] -> IO [Index]
insertStagedArt conn = returning conn insertArticleStgStmt

-- insertStagedArt stores a set of articles and returns the ids the database
-- assigns to these articles it stores

-- What are these stored article ids? How many articles did you store?

{--
>>> pac <- readSample (dir ++ "sample.json")
>>> length $ rows pac
100
>>> withConnection (\conn -> insertStagedArt conn (rows pac) >>= print)
[Idx 1,Idx 2,Idx 3,Idx 4,Idx 5,Idx 6,Idx 7,...]
--}
