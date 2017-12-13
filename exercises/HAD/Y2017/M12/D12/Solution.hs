{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D12.Solution where

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

import Y2017.M11.D20.Solution hiding (keyphrasesStmt, keyphrase4, recs', recs)

{--
We do not want to make separate SQL calls for each article id. Instead, we want
to batch all our article ids into one query. So, let's rewrite the query so that
we send all the SQL in one shot.
--}

import Control.Arrow ((&&&))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Control.Scan.Config (home)
import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed

import Y2017.M11.D01.Solution (readSpecialChars)
import Y2017.M11.D03.Solution -- for Keyphrase
import Y2017.M11.D07.Solution -- for Recommendation
import Y2017.M11.D09.Solution (fetchRecommend)
import Y2017.M11.D13.Solution -- for KeyWord
import Y2017.M11.D16.Solution (ranked, kw2kw)
import Y2017.M11.D17.Solution -- for KWtable
import Y2017.M11.D21.Solution (rec2brief)
import Y2017.M11.D24.Solution (recs4)
import Y2017.M12.D05.Solution (keywordKeyphraseDict)
import Y2017.M12.D06.Solution (filterMinusRecs)

keyphrasesStmt :: Query
keyphrasesStmt =
   [sql|SELECT a.article_id,a.keyphrase_id,k.strength,k.keyphrase
        FROM keyphrase k LEFT JOIN article_keyphrase a
        ON a.keyphrase_id = k.id
        WHERE a.article_id IN ?|]

keyphrase4 :: Connection -> [Integer] -> IO [(Integer, Integer, Double, String)]
keyphrase4 conn = query conn keyphrasesStmt . Only . In

{-- BONUS -----------------------------------------------------------------

So, with the new one-query keyphrase-fetch, let's rebuild the app using that
instead. That is to say: rebuild recs'
--}

recs' :: Connection -> [KeyWord] -> KWtable -> IO [Recommendation]
recs' conn kws table = do
   idxn <- filterArts conn table kws
   let arts = intersects (Map.elems idxn)
   ixkphrs' <- keyphrase4 conn (Set.toList arts)

-- we need to groupBy here

   let groups = groupBy ((==) `on` fst) (map tup42pair ixkphrs')
   let kwmap = Map.fromList (map (fst . head &&& map snd) groups)
   let scores = map ranked2Score (ranked (kw2kw kwmap idxn) kws arts)
   recs <- fetchRecommend conn (Set.toList arts)
   return (collectResult kwmap recs scores)

tup42pair :: (Integer, Integer, Double, String) -> (Integer, Keyphrase)
tup42pair (artid, _kphrid, str, phr) = (artid,KW str (SQS phr))

main' :: [String] -> IO ()
main' (artid:keywords) = do
   homeDir <- home
   special <- readSpecialChars (homeDir ++ "/.specialChars.prop")
   let src = read artid
   withConnection (\conn ->
      recs4 conn src                     >>= \related ->
      keywordKeyphraseDict conn keywords >>=
      recs' conn keywords                >>=
      BL.putStrLn . encodePretty
                  . map (rec2brief special)
                  . filterMinusRecs src related)

main' _ = putStrLn (unlines ["","cold_filtered <artId> [keyword1,...]", "",
   "\tfilters articles with keywords, then removes already recommended",
   "\tarticles for artId",""])

-- time the old filtered approach (see Y2017.M12.D06) verses this approach for
-- article_id 6495 and the sole keyword: mars

-- now time both approaches for article_id 6495 and the sole keyword: trump
