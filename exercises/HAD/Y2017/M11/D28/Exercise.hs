{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D28.Exercise where

{--
Just as yesterday where we added a set of recommended articles, today we are
going to delete a set of articles.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Store.SQL.Connection
import Store.SQL.Util.Indexed

deleteRecsStmt :: Query
deleteRecsStmt =
   [sql|DELETE FROM recommendation WHERE recommended_article_id IN ?|]

deleteRec :: Connection -> [Integer] -> IO ()
deleteRec conn recs = undefined

{-- BONUS -----------------------------------------------------------------

Write a program that takes a list of recommended article ids to delete and
deletes them.

--}

main' :: [String] -> IO ()
main' artIds = undefined

{-- BONUS-BONUS -----------------------------------------------------------

This may not be your cup of tea, but here it is: write a PHP script that calls
this program with the _REQUEST arguments, then have that PHP script call
start on the source article id to return JSON of all the entries of the
recommendation table for that source article.
--}
