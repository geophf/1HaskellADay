{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Y2017.M11.D20.Solution where

{--
Okay, let's build our query ... IN SQL!

Grab the keywords, then, from a keyword search, return the article set.
--}

import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as MT

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D03.Solution -- for Keyphrase
import Y2017.M11.D06.Solution -- for Score (idx, title, score)
import Y2017.M11.D07.Solution hiding (recs) -- for marry
import Y2017.M11.D08.Solution -- for Recommend-type and output of JSON
import Y2017.M11.D09.Solution -- for fetching articles (recommendations)
import Y2017.M11.D13.Solution -- for mapping keywords to articles
import Y2017.M11.D16.Solution -- for ranking
import Y2017.M11.D17.Solution -- for grabbing the kws

-- step 1: grab the keywords

kwTable :: Connection -> IO KWtable
kwTable conn = kwTable' <$> fetchKW conn

kwTable' :: [IxKeys] -> KWtable
kwTable' = MT.start . map (idx &&& val)

{--
>>> connectInfo 
ConnectInfo {connectHost = "...",...}
>>> conn <- connect it
>>> kwtable <- kwTable conn
>>> length (MT.readIndex kwtable)
28586
--}

-- Now, with this query

filterArtsStmt :: Query
filterArtsStmt = [sql|SELECT keyword_id,article_id
                      FROM article_keyword
                      WHERE keyword_id IN ?|]

{-- 
We have to look at an efficient join here, I think ...
Originally I did n SQL lookups, 1 lookup per keyword. Now I rewrote the SQL
to return the keyword (ids) with the article ids en masse
--}

filterArts :: Connection -> KWtable -> [KeyWord] -> IO IndexedArticles
filterArts conn kws vals =
   Map.fromList
   . map ((MT.fromTable kws Map.!) . fst . head &&& Set.fromList . map snd)
   . groupBy ((==) `on` fst) . map ix2tup <$> filterKwArt conn kws vals

filterKwArt :: Connection -> KWtable -> [KeyWord] -> IO [IxValue Integer]
filterKwArt conn (MT.readIndex -> kws) keys =

-- first: we have to bail on any nixed keywords:

   let keyids = map (`Map.lookup` kws) keys in
   if any (== Nothing) keyids then return []

-- then: we match keywords to articles:

   else query conn filterArtsStmt (Only (In (catMaybes keyids)))

{--
>>> idxn <- filterArts conn kwtable (words "trump election")
>>> length idxn
2

idxn is IndexedArticles

>>> arts = intersects $ Map.elems idxn
>>> length arts
80

arts is Set ArticleId
--}

-- Now, let's translate that to articles with key-phrases ... that is to say:
-- to our JSON structure we've returned before.

-- step 2: get the full articles back using fetchRecommend from the filtered
-- article Id-set.

-- step 3: get the key-phrases for article x

keyphrasesStmt :: Query
keyphrasesStmt = 
   [sql|SELECT id,strength,keyphrase FROM keyphrase WHERE id IN 
        (SELECT keyphrase_id FROM article_keyphrase WHERE article_id=?)|]

keyphrase4 :: Connection -> Integer -> IO [IxPhrase]
keyphrase4 conn artId = map xphr2IxPhr <$> query conn keyphrasesStmt [artId]

{-- 
>>> head $ Set.toList arts
125
>>> kphr125 <- keyphrase4 conn (head $ Set.toList arts)
>>> length kphr125 
15
>>> ixkphrs' <- mapM (keyphrase4 conn) (Set.toList arts)  -- SLOW!
>>> ixkphrs = Map.fromList (zip (Set.toList arts) ixkphrs')
--}

-- Now we marry keywords to articles and return the ranked article set.

{--
>>> kwmap = Map.map (map val) ixkphrs

kwmap is KeywordMap

>>> kwarts = searchKeymap kwmap

kwarts is IndexedArticles

>>> r = ranked (kw2kw kwmap idxn) (words "trump election") arts

r is [(ArticleId, Strength)]
--}

ranked2Score :: (ArticleId, Strength) -> Score
ranked2Score (i,s) = Row i "title" (VAL s)

{--
Move to Recommend module
instance Indexed Recommend where
   idx (Rec i _ _ _ _) = read i
--}

-- updates the recommendations with their scores and keywords

collectResult :: KeyphraseMap -> [Recommend] -> [Score] -> [Recommendation]
collectResult kphrs recs scores =
   map (\rec -> rec { scoreKWs = kphrs Map.! fromIntegral (scoreIdx rec) })
   $ marry (Map.fromList $ map (idx &&& id) recs)
           (Map.fromList $ map (idx &&& id) scores)

{--
>>> scores = map ranked2Score r
>>> recs <- fetchRecommend conn (Set.toList arts)

Okay, that's enough commands to package up into an application:
--}

-- so we need a function that from a set of keywords we return a set of
-- filtered articles.

recs :: Connection -> [KeyWord] -> IO [Recommendation]
recs conn keywords = kwTable conn >>= recs' conn keywords

-- we'll look at improving recs in a future 1HaskellADay exercise

recs' :: Connection -> [KeyWord] -> KWtable -> IO [Recommendation]
recs' conn keywords kwtable = do
   idxn <- filterArts conn kwtable keywords
   let arts = intersects (Map.elems idxn)
   ixkphrs' <- mapM (keyphrase4 conn) (Set.toList arts)
   let ixkphrs = Map.fromList (zip (Set.toList arts) ixkphrs')
   let kwmap = Map.map (map val) ixkphrs
   let kwarts = searchKeymap kwmap
   let r = ranked (kw2kw kwmap idxn) keywords arts
   let scores = map ranked2Score r
   recs <- fetchRecommend conn (Set.toList arts)
   return (collectResult kwmap recs scores)

main' :: [KeyWord] -> IO ()
main' kw = withConnection (flip recs kw >=> BL.putStrLn . encodePretty)
