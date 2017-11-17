{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D17.Exercise where

{--
Yesterday, we were able to associate keywords to key-phrases and therefore
rank articles by their affinity to the search keywords.

Great!

The approach yesterday was an off-line one. Today we want to upload the keywords
into our PostgreSQL database do all the linkages in the SQL paradigm, so the
keyword search, going forward, is already materialized in the database, and
not a set of associations we need to generate ourselves each time we fire up the
old Haskell engine.
--}

import Data.Map (Map)
import Data.Set (Set)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as MT

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D03.Exercise -- for Keyphrase
import Y2017.M11.D13.Exercise -- for IndexedArticles

type KWtable = MemoizingTable Integer String
type IxKeys = IxValue String

-- FETCHING OLD KEYWORDS ----------------------------------------------------

fetchKWsStmt :: Query
fetchKWsStmt = [sql|SELECT * FROM keyword|]

fetchKW :: Connection -> IO [IxKeys]
fetchKW conn = undefined

-- init your memoizing table with the keywords, then partition pre-stored
-- keywords and new keywords. Add the new keywords to the datastore and get
-- back the new indicies then update the memoizing table

-- STORING NEW KEYWORDS ----------------------------------------------------

addNewKWsStmt :: Query
addNewKWsStmt = [sql|INSERT INTO keyword (keyword) VALUES (?) RETURNING id|]

addNewKW :: Connection -> KWtable -> IO [Index]
addNewKW conn sym = undefined

-- hint: see triage and updating functionality in memoizing table module

-- At this point, your memoizing table is up-to-date with all KeyWords having
-- indices. MAKE IT SO, NUMBER ONE!

{-- MARRYING KEYPHRASES ----------------------------------------------------

Now here is the meat of today's exercise. Let's say you've stored a set of
key-phrases on a new set of articles you've processed ('processed' meaning
'extracted the key-phrases from') under a key-phrase 'kind' (therefore 
versioning the key-phrase insert). So, we're going to fetch those key-phrases
with their ids, associate these keywords with the indexed key-phrases.

Then, from Y2017.M11.D13, we have IndexedArticles, and, as we have the ids of
keywords, we can join article ids to keyword ids via the memoizing table.
--}

-- JOINING KEYWORDS AND KEYPHRASES -----------------------------------------

instance FromRow Keyphrase where
   fromRow = undefined

fetchKPhrsStmt :: Query
fetchKPhrsStmt =
   [sql|SELECT id,strength,keyphrase FROM keyphrase WHERE kind = ?|]

type IxPhrase = IxValue Keyphrase

fetchKphr :: Connection -> Index -> IO [IxPhrase]
fetchKphr conn kind = undefined

-- Okay, so now we need to marry keyword IDS to key-phrase IDS and pivot

storeKwKphrJoin :: Query
storeKwKphrJoin =
   [sql|INSERT INTO keyphrase_keyword (keyphrase_id,keyword_id) VALUES (?,?)|]

-- so now we build a pivot table that for each keyphrase we compute the set
-- of keyword ids associated with the words of the keyphrase

keywords4 :: KWtable -> IxPhrase -> [(Integer, Integer)]
keywords4 keywords keyphrase = undefined

-- we do that for all the key-phrases and we have our insert

storeKwKphr :: Connection -> KWtable -> [IxPhrase] -> IO ()
storeKwKphr conn kws phrases = undefined 

{-- JOINING KEYWORDS AND ARTICLES --------------------------------------------

From the IndexedArticles we know keywords to [ArticleId], and from KWtable
we know keyword -> Idx. So now we just store (kwIdx,ArticleId)
--}

storeKwArtJoin :: Query
storeKwArtJoin =
   [sql|INSERT INTO article_keyword (keyword_id,article_id) VALUES (?,?)|]

unwindKwArt :: KWtable -> IndexedArticles -> [(Integer,ArticleId)]
unwindKwArt kwTable kwArts = undefined

-- you can tell I come from Smalltalk: 'unwindDo: '

storeKwArt :: Connection -> KWtable -> IndexedArticles -> IO ()
storeKwArt conn kwtable kwarts = undefined

-- COOL!

-- Next week we'll retrieve articles from the database querying by keyword and
-- packaging that information as JSON
