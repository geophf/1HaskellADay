{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D12.Exercise where

{--
Okay, I came across an error when I was filtering articles with indexed 
keywords:

time cold_filtered 6495 trump
cold_filtered: UnexpectedNull {errSQLType = "text",
      errSQLTableOid = Just (Oid 50231), errSQLField = "summary",
      errHaskellType = "Text", errMessage = ""}

What this is saying is that there are articles that do not have summaries, and
so the type for summary, String, is not appropriate. I changed the type to
Maybe String, and that fixes the error, but now we are recommending articles
that do not have summaries. That's not good.

So, today's Haskell problem is to filter out articles that do not have 
summaries... shoot! Which I already did, since a brief, intrinsic to its type
needs a summary.

So, instead we have a different problem. Filtering by keyword is slow, not 
because the filtering is slow, but when we go back to assign a ranking we fetch
the key-phrase strength for each of the articles, ... one-by-one. See:
--}

import Y2017.M11.D20.Exercise hiding (keyphrasesStmt, keyphrase4)

{--
We do not want to make separate SQL calls for each article id. Instead, we want
to batch all our article ids into one query. So, let's rewrite the query so that
we send all the SQL in one shot.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D03.Exercise -- for Keyphrase
import Y2017.M11.D07.Exercise -- for Recommendation
import Y2017.M11.D13.Exercise -- for KeyWord
import Y2017.M11.D17.Exercise -- for KWtable

keyphrasesStmt :: Query
keyphrasesStmt =
   [sql|SELECT a.article_id,a.keyphrase_id,k.strength,k.keyphrase
        FROM keyphrase k LEFT JOIN article_keyphrase a
        ON a.keyphrase_id = k.id
        WHERE a.article_id IN ?|]

keyphrase4 :: Connection -> [Integer] -> IO [(Integer, Integer, Double, String)]
keyphrase4 conn artids = undefined

{-- BONUS -----------------------------------------------------------------

So, with the new one-query keyphrase-fetch, let's rebuild the app using that
instead. That is to say: rebuild recs'
--}

recs' :: Connection -> [KeyWord] -> KWtable -> IO [Recommendation]
recs' conn kws table = undefined

main' :: [String] -> IO ()
main' (artid:keywords) = undefined

-- hints:
-- look at Y2017.M11.D20 for definition of recs'
-- look at Y2017.M12.D06 for filtered on article id keyword searches

-- time the old filtered approach (see Y2017.M12.D06) verses this approach for
-- article_id 6495 and the sole keyword: mars

-- now time both approaches for article_id 6495 and the sole keyword: trump
