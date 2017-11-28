{-# LANGUAGE OverloadedStrings, QuasiQuotes, TupleSections #-}

module Y2017.M11.D28.Solution where

{--
Just as yesterday where we added a set of recommended articles, today we are
going to delete a set of articles.
--}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D06.Solution

{--
Actually, we want to build the query dynamically, otherwise we're executing
multiple delete statements, thus:

deleteRecsStmt :: Query
deleteRecsStmt =
   [sql|DELETE FROM recommendation
        WHERE for_article_id=? AND recommended_article_id=?|]

deleteRec :: Connection -> Integer -> [Integer] -> IO ()
deleteRec conn srcId recIds =
   mapM_ (execute conn deleteRecsStmt)
                    (sequence (srcId, recIds))

we don't want this. So let's build a delete statement with the source article
id embedded into the the query then delete the recommended articles en masse.
--}

deleteRec :: Connection -> Integer -> [Integer] -> IO ()
deleteRec conn srcId =
   let query = "DELETE FROM recommendation WHERE for_article_id=" ++ show srcId
            ++ " AND recommended_article_id IN ?" in
   void . execute conn (Query (B.pack query)) . Only . In

{-- BONUS -----------------------------------------------------------------

Write a program that takes a list of recommended article ids to delete and
deletes them.
--}

main' :: [String] -> IO ()
main' artIds@(h:t1:t2) =
   withConnection (\conn -> deleteRec conn (read h) (map read (t1:t2)))
main' _ = putStrLn (unlines ["","remover <srcId> <artId1> [artId2 ...]","",
   "\tRemoves <artIds> associated to <srcId> from the recommendation table.",
   ""])

{-- BONUS-BONUS -----------------------------------------------------------

This may not be your cup of tea, but here it is: write a PHP script that calls
this program with the _REQUEST arguments, then have that PHP script call
start on the source article id to return JSON of all the entries of the
recommendation table for that source article.
--}
