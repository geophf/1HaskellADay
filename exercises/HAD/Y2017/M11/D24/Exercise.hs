{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D24.Exercise where

{--
Let's say, oh, that you're in a start-up, and you would like to, oh, return
a list of articles associated with a source article as JSON from a PostgreSQL
database.

Hypothetically.

For example.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository.

import Control.Scan.Config
import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D01.Exercise   -- for SpecialCharTable
import Y2017.M11.D03.Exercise   -- for Strength
import Y2017.M11.D21.Exercise   -- for Brief

{--
This is the Brief format, but instead of filtering down to a set of articles,
these articles, and their scores are already provided for you in the database.

So this exercise eta-reduces to writing a FromRow-ish instance on Brief, 
extracting data from multiple tables then cleaning up special characters as
we go.
--}

recs4Stmt :: Query
recs4Stmt = [sql|SELECT recommended_article_id,hindsight_score
                 FROM recommendation WHERE article_id=?|]

recs4 :: Connection -> Integer -> IO [IxValue (Maybe Strength)]
recs4 conn artId = undefined

-- now, we need to get the rest of the data, given the recommendation ids,
-- from the article table

articleDataStmt :: Query
articleDataStmt = [sql|SELECT id,publish_dt,title,full_txt FROM article
                       WHERE id IN ?|]

-- then we marry the IxValue with the data returned here to get a Brief

articleData :: SpecialCharTable -> Connection -> [IxValue (Maybe Strength)] 
            -> IO [Brief]
articleData conn artIds = undefined

-- now we have to return those as JSON ... but we already know how to do that.

{-- BONUS -----------------------------------------------------------------

So, create an app that from an article id returns the set of recommended 
articles as JSON
--}

main' :: [String] -> IO ()
main' args = undefined
