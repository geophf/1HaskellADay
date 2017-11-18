{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Y2017.M11.D17.Solution where

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

import Control.Arrow ((&&&), (***))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as MT

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts

import Y2017.M11.D03.Solution -- for Keyphrase
import Y2017.M11.D13.Solution -- for IndexedArticles

type KWtable = MemoizingTable Integer String
type IxKeys = IxValue String

-- FETCHING OLD KEYWORDS ----------------------------------------------------

fetchKWsStmt :: Query
fetchKWsStmt = [sql|SELECT * FROM keyword|]

fetchKW :: Connection -> IO [IxKeys]
fetchKW = flip query_ fetchKWsStmt

{--
>>> connectInfo 
ConnectInfo {connectHost = "...",...}
>>> conn <- connect it
>>> keys <- fetchKW conn
>>> length keys
0
--}

-- init your memoizing table with the keywords, then partition pre-stored
-- keywords and new keywords. Add the new keywords to the datastore and get
-- back the new indicies then update the memoizing table

{--
>>> kwmt = MT.initMemTable (Map.empty, Map.empty)
--}

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

{--
instance FromRow Keyphrase where
   fromRow = KW <$> field <*> (SQS <$> field)
--}

fetchKPhrsStmt :: Query
fetchKPhrsStmt =
   [sql|SELECT id,strength,keyphrase FROM keyphrase WHERE kind = ?|]

type IxPhrase = IxValue Keyphrase

data TransPhrase = XPhr Integer Strength String

instance FromRow TransPhrase where
   fromRow = XPhr <$> field <*> field <*> field

xphr2IxPhr :: TransPhrase -> IxPhrase
xphr2IxPhr (XPhr i s st) = IxV i (KW s (SQS st))

fetchKphr :: Connection -> Index -> IO [IxPhrase]
fetchKphr conn kind = map xphr2IxPhr <$> query conn fetchKPhrsStmt [kind]

{--
>>> kphr <- fetchKphr conn (Idx 1)
>>> length kphr
218633
>>> head kphr 
IxV {ix = 1, val = KW {strength = 7.40909090909091,
                       keyphrase = SQS {string = "frequent binge drinking"}}}

-- updating the keyword memoizing table:

>>> newkwmt = foldr MT.triageMT kwmt (invert (map val kphr))
>>> length (MT.newValues newkwmt)
28586
--}

-- STORING NEW KEYWORDS ----------------------------------------------------

addNewKWsStmt :: Query
addNewKWsStmt = [sql|INSERT INTO keyword (keyword) VALUES (?) RETURNING id|]

data Kywrd = K String

instance ToRow Kywrd where
   toRow (K s) = [toField s]

addNewKW :: Connection -> KWtable -> IO [Index]
addNewKW conn = returning conn addNewKWsStmt . map K . Set.toList . MT.newValues

-- hint: see triage and updating functionality in memoizing table module

-- At this point, your memoizing table is up-to-date with all KeyWords having
-- indices. MAKE IT SO, NUMBER ONE!

{--
>>> newkwidxn <- addNewKW conn newkwmt 
>>> newnewkwmt = MT.updateMT (zip (map idx newkwidxn) (Set.toList (MT.newValues newkwmt))) newkwmt

... MT.updateMT needs to change to accept just the keys and use its own set.
--}

-- Okay, so now we need to marry keyword IDS to key-phrase IDS and pivot

storeKwKphrJoin :: Query
storeKwKphrJoin =
   [sql|INSERT INTO keyphrase_keyword (keyphrase_id,keyword_id) VALUES (?,?)|]

-- so now we build a pivot table that for each keyphrase we compute the set
-- of keyword ids associated with the words of the keyphrase

keywords4 :: KWtable -> IxPhrase -> [(Integer, Integer)]
keywords4 (MT.readIndex -> kws) = 
   sequence . (ix &&& mapMaybe (`Map.lookup` kws) . wordsOf)
      where wordsOf = words . string. keyphrase . val

-- we do that for all the key-phrases and we have our insert

storeKwKphr :: Connection -> KWtable -> [IxPhrase] -> IO ()
storeKwKphr conn kws =
   inserter conn storeKwKphrJoin . concatMap (keywords4 kws)

{--
>>> storeKwKphr conn newnewkwmt kphr

in SQL:

$ select count(1) from keyphrase_keyword;
254122

Wow.
--}

{-- JOINING KEYWORDS AND ARTICLES --------------------------------------------

From the IndexedArticles we know keywords to [ArticleId], and from KWtable
we know keyword -> Idx. So now we just store (kwIdx,ArticleId)
--}

storeKwArtJoin :: Query
storeKwArtJoin =
   [sql|INSERT INTO article_keyword (keyword_id,article_id) VALUES (?,?)|]

unwindKwArt :: KWtable -> IndexedArticles -> [(Integer,ArticleId)]
unwindKwArt (MT.readIndex -> kws) =
   concatMap (sequence . ((kws Map.!) *** Set.toList)) . Map.assocs

-- you can tell I come from Smalltalk: 'unwindDo: '

storeKwArt :: Connection -> KWtable -> IndexedArticles -> IO ()
storeKwArt conn kws = inserter conn storeKwArtJoin . unwindKwArt kws

{--
>>> kws <- readCompressedKeyphrases "Y2017/M11/D03/refinedEr_kws.txt.gz" 

... we really should be getting kws from the database, but ...

>>> kwmap = searchKeymap kws
>>> length kwmap
28586

-- let's store this analysis:

>>> storeKwArt conn newnewkwmt kwmap 

in SQL:

$ select count(1) from article_keyword;
238485
--}

-- COOL!

-- Next week we'll retrieve articles from the database querying by keyword and
-- packaging that information as JSON
