{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M01.D31.Exercise where

{--
Yesterday we downloaded the appropriate number of packets from the REST 
endpoint, and, comparing the articles in the packets to the metadata of the
articles in the database, we were able to triage the downloaded articles.

Today, we'll take the triaged articles, along with the metadata, and either
upload or update the articles in the database.
--}

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Data.Maybe (mapMaybe)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Data.HTML -- (htmlBlock, plainText, demark)
import Data.Logger (Logger)

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M12.D20.Exercise (Packet, Block)
import Y2017.M12.D26.Exercise (insertStagedArt)
import Y2017.M12.D27.Exercise (insertArts, DatedArticle, prologue, lastupdated)

-- import Y2018.M01.D15.Exercise (elt) -- hint for etl process

import Y2018.M01.D10.Exercise (gruntWerk)
import Y2018.M01.D26.Exercise -- for downloading packets from REST endpoint
import Y2018.M01.D29.Exercise (oneWeekAgo)
import Y2018.M01.D30.Exercise (ArticleTriageInformation, Triage)

-- for uploading the articles see the process of the gruntWerk for uploading
-- the staged articles then the articles, themselves.

-- Do that.

-- So, now, let's look at the updating issue. We're NOT going to change anything
-- other than the article table (oops, and the block table, which we can get
-- when we update the article table)

updateArtStmt :: IxValue (DatedArticle a) -> Query
updateArtStmt (IxV idx art) =
   let params = mapMaybe sequence [("abstract", demark <$> prologue art),
         ("update_dt", show <$> lastupdated art),
         ("full_text", Just (unlines $ plainText art)),
         ("rendered_text", Just (htmlBlock art))]
       rendered = concatMap (\(a,b) -> (' ':a ++ "='" ++ b ++ "'")) params
   in Query (B.pack ("UPDATE article SET" ++ rendered
                   ++ "WHERE id=" ++ show idx ++ " returning src_id"))

updateArts :: Connection -> [IxValue (DatedArticle a)] -> IO [Index]
updateArts conn arts = undefined

-- now we need to update the source blocks:

updateBlockStmt :: IxValue Block -> Query
updateBlockStmt (IxV idx blk) =
   Query (B.pack ("UPDATE article_stg SET block='"
               ++ BL.unpack (encodePretty blk)
               ++ "' WHERE id=" ++ show idx))

updateBlocks :: Connection -> [IxValue Block] -> IO ()
updateBlocks conn blocks = undefined

-- so, we need to figure out how to get from the ArticleTriageInformation
-- to a set of indexed articles (that return block indices) then to a set
-- of indexed blocks ... make sure the articles are sorted going in so that
-- the sorted results coming out match the block ids (I learned that one the
-- hard way!

insertAndUpdate :: Connection -> Map Triage [ArticleTriageInformation] 
                -> StampedWriter LogEntry [IxValue (DatedArticle Authors)]
insertAndUpdate conn triage = undefined

{-- BONUS -----------------------------------------------------------------

Now, put this all together to create the daily upload process
--}

dailyUpload :: Connection
            -> StampedWriter LogEntry (Maybe (Packet, IxValue (DatedArticle Authors)))
dailyUpload conn = undefined

-- reminders:
-- * get the oneWeekAgo value
-- * download the packets from the REST endpoint
-- * triage the articles against what's in the database
-- * create the megapacket from the triaged articles
-- * log entries
-- * storeArticles
-- * update articles
-- * audit entry

-- some (or all?) of this functionality may be handled for you already
-- by functions we've defined in exercises prior.

main' :: [String] -> IO ()
main' args = undefined
