{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D10.Exercise where

{--
So we've got keywords and recommended article sets as files we're reading from.

El-great-o.

Now, let's create a scheme and upload these data to our data store.
--}

import Data.Map (Map)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable
import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts
import Store.SQL.Util.Pivots

import Y2017.M11.D03.Exercise  -- Keyphrase
import Y2017.M11.D06.Exercise  -- Score, etc
import Y2017.M11.D07.Exercise  -- Recommend

-- We will read in our keyphrases and scores/recommendations and store them in 
-- the database

-- step 1. read in the keyphrases

-- step 2. read in the scores

{--
-- Now we have to provide ToRow instances of these things

instance ToRow Keyphrase where
   toRow kw = undefined

On second thought, is that the way to go? Storing a keyphrase requires the 
below --v keyphrase kind, so a toRow instance of Keyphrase doesn't make sense, 
as a row's information comes from multiple sources.
--}

-- each time we save a keyphrase set, we provide versioning/typing information 
-- on how we got our keywords. Was it Maui, TFIDF, word-frequency? Let's add a 
-- row to say that.

keyphraseKindStmt :: Query
keyphraseKindStmt = [sql|INSERT INTO keyphrase_algorithm (algorithm)
                         VALUES (?) RETURNING id|]

keyphraseKind :: Connection -> String -> IO [Index]
keyphraseKind conn kind = undefined

-- hint: how do we pass kind in the format Simple SQL expects?

-- So, we have the keyphrases, their strengths, and an index for kind, let's put
-- these together and insert each keyphrase into the keyword database, linking 
-- the keyphrase to the article_id ... which, of course, is linked via 
-- article_analysis

-- so we do need to know what articles are analyzed already, and which articles 
-- are not ... enter the memoizing table

-- first, load article_analysis into the memoizing table by getting its values

articlesStmt :: Query
articlesStmt = [sql|SELECT id,article_id FROM article_analysis|]

article :: Connection -> IO [(Integer, Integer)]
article conn = undefined -- query_ conn articlesStmt

{--
>>> arts <- article conn
>>> arts
[]

... which is fine, as we haven't done any analysis (that we've stored in the 
database) on our article set. So let's do that.

We do that by inserting the keyphrase with its article index into the database, then checked to make sure we have the article id, or, if not, add it to our 
memoizing table.

Goodness this is a lot of housekeeping!
--}

insertKWsStmt :: Query
insertKWsStmt = [sql|INSERT INTO keyphrase (strength,keyphrase,kind) 
                     VALUES (?,?,?) returning id|]

data KWInserter = KWI Strength String Integer

instance ToRow KWInserter where
   toRow kwi = undefined

kws2Kwi :: Index -> KeyphraseMap -> [KWInserter]
kws2Kwi idx kphmap = undefined

insertKW :: Connection -> Index -> KeyphraseMap -> IO [Index]
insertKW conn idx kphrmap = undefined

-- And now we have the keyphrase indices to insert into the join table which 
-- must (and this is where the memoizing table comes in) refer to the article id-- in the article_analysis table. If an article id isn't in the analysis table, -- we add it.

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
keys2indices kphrmap = undefined

insertKWjoins :: Connection -> [Index] -> KeyphraseMap -> IO ()
insertKWjoins conn idxn kphrmap = undefined

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
                        (recommended_article_id,article_id,hindsight_score)
        VALUES (?,?,?)|]

insertStudied :: Connection -> Score -> IO ()
insertStudied conn (Row i _ QRY) = inserter conn insertStudiedStmt [Idx i]

insertRecs :: Connection -> Index -> [Score] -> IO ()
insertRecs conn idx = inserter conn insertRecsStmt
                             . map (\(Row i _ (VAL f)) -> (idx,i,f))

-- which means we need to partition the scores by type.

-- ANALYSES -----------------------------------------------------------------

-- Now we can wrap it up by collating all article ids and inserting the new ones
-- into the article_analysis table

type MTII = MemoizingTable Integer Integer

insertAnalyzedStmt :: Query
insertAnalyzedStmt = [sql|INSERT INTO article_analysis (article_id) VALUES (?)|]

insertAnalyses :: Connection -> MTII -> IO ()
insertAnalyses conn = undefined

-- which, of course, we need to collate these values to insert
-- So we take all the scored recommendations, and all the keywords, and we diff -- them

-- which means we need to make scored indexed by article id

instance Indexed Score where
   idx (Row i _ _) = i

analyzed :: Map Integer Score -> KeyphraseMap -> MTII -> MTII
analyzed scrs kphrmap mtii = undefined

-- but we need a memoizing table to start from ...

startMT :: [(Integer, Integer)] -> MTII
startMT pairs = undefined

-- with that, insert the analyses.
