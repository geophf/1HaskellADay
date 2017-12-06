{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D07.Exercise where

{--
Okay, let's do a hard reset. We have a source article, recommended articles,
then articles to be added, deleted, published ... it's all a big mess now that
we've show a demo, and we want a clean slate.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports are available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)

import Y2017.M11.D06.Exercise    -- for Score and readScoreFile
import Y2017.M11.D09.Exercise (articleSet)
import Y2017.M11.D10.Exercise (insertRecs)

-- first we want to read in our recommended article set: do that.

-- Next, find the source article: that's the QRY scored one, and separate it
-- from the recommended articles

sourceArticleIdx :: [Score] -> (Score, [Score])
sourceArticleIdx scores = undefined

-- Now, clear the recommendation table and the recommendation_publish tables

deleteRecsStmt, deletePubsStmt :: Query
deletePubsStmt = [sql|DELETE FROM recommendation WHERE for_article_id=?|]
deleteRecsStmt =
   [sql|DELETE FROM recommendation_publish WHERE source_article_id=?|]

deletia :: Connection -> Integer -> IO ()
deletia conn srcId = undefined

-- Now, upload the recommendations into the database (see: insertRecs)

{-- BONUS -----------------------------------------------------------------

Build an application that resets for article id x
--}

main' :: [String] -> IO ()
main' [srcIdx] = undefined
