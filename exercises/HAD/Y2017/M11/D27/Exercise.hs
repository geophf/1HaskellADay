{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D27.Exercise where

{--
Okay, we have a set of recommended articles, now we want to add some new 
articles to the list. So, given an article id as a basis and a set of article
ids to add, add those articles to the source article's recommended list.

Yes: it isn't rocket science.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow

import Store.SQL.Connection
import Store.SQL.Util.Pivots

{--
Actually, when you get right down to it: since added articles are not scored,
adding rows to the recommendation table becomes a pivot table exercise. But 
even then, since we're only working with one source article to add articles to,
this is just an insert-in-context.
--}

insertRecsStmt :: Query
insertRecsStmt =
   [sql|INSERT INTO recommendation (for_article_id,recommended_article_id)
        VALUES (?,?)|]

insertRec :: Connection -> Integer -> [Integer] -> IO ()
insertRec conn srcId recs = undefined

{-- BONUS -----------------------------------------------------------------

Write a program that takes a source article ID and a list of recommended
article ids an inserts that set into recommendation.
--}

main' :: [String] -> IO ()
main' artIds = undefined
