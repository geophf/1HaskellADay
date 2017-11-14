{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Y2017.M11.D10.Solution where

{--
So we've got keyphrases and recommended article sets as files we're reading 
from.

El-great-o.

Now, let's create a scheme and upload these data to our data store.
--}

import Control.Arrow ((&&&))
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as MT
import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts
import Store.SQL.Util.Pivots

import Y2017.M11.D03.Solution              -- Keyphrase
import Y2017.M11.D06.Solution              -- Score, Value
import Y2017.M11.D07.Solution              -- Recommend

-- We will read in our keyphrases and scores/recommendations and store them in
-- the database

-- step 1. read in the keyphrases

{--
>>> kws <- readCompressedKeyphrases "Y2017/M11/D03/refinedEr_kws.txt.gz" 
--}

-- step 2. read in the scores

{--
>>> scrs <- readScoreFile "Y2017/M11/D09/article-set.csv" 
--}

{--
-- Now we have to provide ToRow instances of these things

instance ToRow Keyphrase where
   toRow kw = [toField ...

Actually kw is a composite (see below) of keyphrase type (external) strength 
(internal) and the keyphrase, so a ToRow instance doesn't make sense
--}

-- each time we save a keyphrase set, we provide versioning/typing information
-- on how we got our keyphrases. Was it Maui, TFIDF, word-frequency? Let's add a
-- row to say that.

keyphraseKindStmt :: Query
keyphraseKindStmt = [sql|INSERT INTO keyphrase_algorithm (algorithm)
                         VALUES (?) RETURNING id|]

keyphraseKind :: Connection -> String -> IO [Index]
keyphraseKind conn kind = query conn keyphraseKindStmt [kind]

{--
>>> connectInfo 
ConnectInfo {connectHost = "...",...}
>>> conn <- connect it
>>> kind <- keyphraseKind conn "refinedEr_kw set"
>>> kind
[Idx 1]
--}

-- So, we have the keyphrases, their strengths, and an index for kind, let's
-- put these together and insert each keyphrase into the keyphrase database, 
-- linking the keyphrase to the article_id ... which, of course, is linked via 
-- article_analysis

-- so we do need to know what articles are analyzed already, and which articles 
-- are not ... enter the memoizing table

-- first, load article_analysis into the memoizing table by getting its values

articlesStmt :: Query
articlesStmt = [sql|SELECT id,article_id FROM article_analysis|]

article :: Connection -> IO [(Integer, Integer)]
article conn = query_ conn articlesStmt

{--
>>> arts <- article conn
>>> arts
[]

... which is fine, as we haven't done any analysis (that we've stored in the 
database) on our article set. So let's do that.

We do that by inserting the keyphrase with its article index into the database, 
then checked to make sure we have the article id, or, if not, add it to our 
memoizing table.

Goodness this is a lot of housekeeping!
--}

insertKWsStmt :: Query
insertKWsStmt = [sql|INSERT INTO keyphrase (strength,keyphrase,kind)
                     VALUES (?,?,?) returning id|]

data KWInserter = KWI Strength String Integer

instance ToRow KWInserter where
   toRow (KWI a b c) = [toField a, toField b, toField c]

kws2Kwi :: Index -> KeyphraseMap -> [KWInserter]
kws2Kwi (Idx i) = map (\(KW a (SQS b)) -> KWI a b i) . concat . Map.elems

insertKW :: Connection -> Index -> KeyphraseMap -> IO [Index]
insertKW conn idx = returning conn insertKWsStmt . kws2Kwi idx

-- And now we have the keyphrase indices to insert into the join table which 
-- must (and this is where the memoizing table comes in) refer to the article id
-- in the article_analysis table. If an article id isn't in the analysis table, 
-- we add it.

insertKWjoinsStmt :: Query
insertKWjoinsStmt = [sql|INSERT INTO article_keyphrase (keyphrase_id,article_id)
                         VALUES (?,?)|]

-- but since we have the article ids in the keyphrase map already, and we know 
-- what the article ids already stored in the memoizing table, we can treat 
-- this problem at another time.

-- but what we do have to do is convert the map indices (one article_id for many
-- keyphrases) to be in one-for-one correspondence to the returned indices of 
-- the keyphrases just inserted.

keys2indices :: KeyphraseMap -> [Index]
keys2indices = map (Idx . fromIntegral . fst) . concatMap sequence . Map.toList

insertKWjoins :: Connection -> [Index] -> KeyphraseMap -> IO ()
insertKWjoins conn idxs =
   inserter conn insertKWjoinsStmt . zip idxs . keys2indices

{--
>>> kwIds <- insertKW conn (head kind) kws
>>> length kwIds
218633

BOOM! That took a while...

>>> insertKWjoins conn kwIds kws

BOOM! That was fast!
--}

-- RECOMMENDATIONS -------------------------------------------------------

-- Now we do the same with the recommendation set. Also memoizing article ids
-- from the recommendation data structure...

{--
We can't have a single ToRow instance because the recommendations are divided
amongst two tables: QRY goes to article_recommendation and VAL x goes to 
recommendation

instance ToRow Recommend where
   toRow r = undefinek
--}

insertStudiedStmt :: Query
insertStudiedStmt =
   [sql|INSERT INTO article_recommendation (article_id) VALUES (?)|]

insertRecsStmt :: Query
insertRecsStmt =
   [sql|INSERT INTO recommendation
                        (recommended_article_id,for_article_id,hindsight_score)
        VALUES (?,?,?)|]

insertStudied :: Connection -> Score -> IO ()
insertStudied conn (Row i _ QRY) = inserter conn insertStudiedStmt [Idx i]

insertRecs :: Connection -> Index -> [Score] -> IO ()
insertRecs conn idx = inserter conn insertRecsStmt
                             . map (\(Row i _ (VAL f)) -> (i,idx,f))

-- which means we need to partition the scores by type:

{--
>>> let (seed, recs) = partition ((== QRY) . score) (Map.elems scrs)
>>> seed
[Row {idx = 6945,
      title = "Trump Resurrects His Claim That Both Sides Share Blame in Charlottesville Violence",
      score = QRY}]

... and now we can do our inserts:

>>> insertStudied conn (head seed)

but 'id' is not what we want at all! We want the article_id of the studied 
article. DUH!

>>> idx (head seed)
6945

THERE we go! (fixing the query to return null)

... and with that, we can insert our recommendations:

>>> insertRecs conn (Idx . idx $ head seed) recs

... and on the SQL-side:

$ select * from recommendation LIMIT 5;

id	recommended_article_id	article_id	score
1	6495			1733		96.0
2	6495			1778		87.0
3	6495			1790		89.0
4	6495			1886		78.0
5	6495			1891		93.0

Oops! WRONG! BACKWARDS! ... and it only took all this time to notice this smh!
--}

-- ANALYSES -----------------------------------------------------------------

-- Now we can wrap it up by collating all article ids and inserting the new ones
-- into the article_analysis table

type MTII = MemoizingTable Integer Integer

insertAnalyzedStmt :: Query
insertAnalyzedStmt = [sql|INSERT INTO article_analysis (article_id) VALUES (?)|]

insertAnalyses :: Connection -> MTII -> IO ()
insertAnalyses conn =
   inserter conn insertAnalyzedStmt . map Idx . Set.toList . MT.newValues

-- which, of course, we need to collate these values to insert
-- So we take all the scored recommendations, and all the keywords, and we diff 
-- them

-- which means we need to make scored indexed by article id

{--
Move this instance to the Score module
instance Indexed Score where
   idx (Row i _ _) = i
--}

analyzed :: Map Integer Score -> KeyphraseMap -> MTII -> MTII
analyzed (Map.keys -> scrs) (map fromIntegral . Map.keys -> kws) =
   flip (foldr MT.triage) (scrs ++ kws)

-- but we need a memoizing table to start from ...

startMT :: [(Integer, Integer)] -> MTII
startMT = MT.init . adjoin Map.fromList . (id &&& map swap)

{--
>>> mtii = analyzed scrs kws (startMT arts)
>>> length $ fromTable mtii
9835

... because I've run this exercise before ... again!

... startMT needs to be moved to Data.MemoizingTable

>>> length (newValues mtii)
0
>>> insertAnalyses conn mtii

... returns instantly, no rows to add.

... and on the SQL side:

$ select * from article_analysis LIMIT 5;

id	article_id
1	1
2	2
3	3
4	4
5	5
--}
