{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Y2017.M11.D20.Exercise where

{--
Okay, let's build our query ... IN SQL!

Grab the keywords, then, from a keyword search, return the article set.
--}

import Data.Map (Map)
import Data.Set (Set)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable)

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D03.Exercise -- for Keyphrase
import Y2017.M11.D06.Exercise hiding (idx) -- for Score (idx, title, score)
import Y2017.M11.D07.Exercise -- for marry (map integer recommend map integer sc
import Y2017.M11.D08.Exercise -- for Recommend-type and output of JSON
import Y2017.M11.D09.Exercise -- for fetching articles (recommendations)
import Y2017.M11.D13.Exercise -- for mapping keywords to articles
import Y2017.M11.D16.Exercise -- for ranking
import Y2017.M11.D17.Exercise -- for grabbing the kws and keyphrases

-- step 1: grab the keywords

kwTable :: Connection -> IO KWtable
kwTable conn = undefined  -- hint: use kwTable' to marshall the memoizing table

kwTable' :: [IxKeys] -> KWtable
kwTable' keys = undefined  -- hint: see functions from D17
-- Now, with this query

filterArtsStmt :: Query
filterArtsStmt = [sql|SELECT keyword_id,article_id
                      FROM article_keyword
                      WHERE keyword_id IN ?|]

-- grab the article IDs that are the intersection of the results:

filterArts :: Connection -> KWtable -> [KeyWord] -> IO IndexedArticles
filterArts conn kws vals = undefined

-- Now, let's translate that to articles with key-phrases ... that is to say:
-- to our JSON structure we've returned before.

-- step 2: get the full articles back using fetchRecommend from the filtered
-- article Id-set.

-- step 3: get the key-phrases for article x

type KPhrMap = Map Integer [Keyphrase]

keyphrasesStmt :: Query
keyphrasesStmt =
   [sql|SELECT id,strength,keyphrase FROM keyphrase WHERE id IN
        (SELECT keyphrase_id FROM article_keyphrase WHERE article_id=?|]

keyphrase4 :: Connection -> Integer -> IO [IxPhrase]
keyphrase4 conn artId = undefined

-- Now we marry keywords to articles and return the ranked article set.

ranked2Score :: (ArticleId, Strength) -> Score
ranked2Score (i,s) = undefined

-- these indexed instances will be helpful marrying scores to articles:

instance Indexed Score where
   idx score = undefined

instance Indexed Recommend where
   idx rec = undefined

collectResult :: KeyphraseMap -> [Recommend] -> [Score] -> [Recommendation]
collectResult kphrs recs scores = undefined

-- hint: see marry and showRecs for guidance

-- What does the JSON look like for a set of articles on "trump" "election"?

-- so we need a function that from a set of keywords we return a set of
-- filtered articles.

recs :: Connection -> [KeyWord] -> IO [Recommendation]
recs conn keywords = undefined    -- hint use recs' after you get a kwtable

recs' :: Connection -> [KeyWord] -> KWtable -> IO [Recommendation]
recs' conn keywords kwtable = undefined

{-- BONUS -----------------------------------------------------------------

Package all of this together as an application and from a list of keywords
return a set of JSON of the filtered articles, ranked.
--}

main' :: [String] -> IO ()
main' keywords = undefined
