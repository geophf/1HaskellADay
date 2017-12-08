{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D07.Solution where

{--
Okay, let's do a hard reset. We have a source article, recommended articles,
then articles to be added, deleted, published ... it's all a big mess now that
we've show a demo, and we want a clean slate.
--}

import Control.Arrow (first)
import Control.Monad (void)
import Data.List (partition)
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports are available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed

import Y2017.M11.D06.Solution    -- for Score and readScoreFile
import Y2017.M11.D09.Solution (articleSet)
import Y2017.M11.D10.Solution (insertRecs)

-- first we want to read in our recommended article set

-- Next, find the source article: that's the QRY scored one, and separate it
-- from the recommended articles

sourceArticleIdx :: [Score] -> (Score, [Score])
sourceArticleIdx = first head . partition ((==QRY) . score)

-- Now, clear the recommendation table and the recommendation_publish tables

deleteRecsStmt, deletePubsStmt :: Query
deletePubsStmt = [sql|DELETE FROM recommendation WHERE for_article_id=?|]
deleteRecsStmt =
   [sql|DELETE FROM recommendation_publish WHERE source_article_id=?|]

deletia :: Connection -> Index -> IO ()
deletia conn srcId =
   void (execute conn deleteRecsStmt [srcId] >>
         execute conn deletePubsStmt [srcId])

-- Now, upload the recommendations into the database (see: insertRecs)

{-- BONUS -----------------------------------------------------------------

Build an application that resets recommendation sets based on a CSV score file.
--}

main' :: [String] -> IO ()
main' [scoreFile] = do
   scores <- readScoreFile scoreFile
   let (scrId, recs) = first (Idx . idx) (sourceArticleIdx (Map.elems scores))
   withConnection (\conn -> deletia conn scrId >> insertRecs conn scrId recs)
   putStrLn "Reset and reloaded the recommendation table."
main' _ = putStrLn (unlines ["","hard_reset <scored_articles_file.csv>","",
   "\tResets the recommendation and recommendation_publish tables;",
   "\tReloads the recommendation table with the top 15 recommended articles"])
