{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M10.D03.Exercise where

{--
Today we're going to look at the MemoizingTable-type.

What is the MemoizingTable-type?

Well, it's what we're going to look at today!

(circular-reference, much, geophf?
me: what?)
--}

import Control.Monad (void)
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Data.Set (Set)
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as Mem

import Store.SQL.Connection (connectInfo)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts
import Store.SQL.Util.Pivots

import Y2017.M09.D25.Exercise (Article)
import Y2017.M10.D02.Exercise (Compressed)

{--
The MemoizingTable type addresses this problem: you have a set of words in a
data table:
--}

data Subject = Subj { subj :: String }
   deriving (Eq, Ord, Show)

-- So, since we have a Subject-type, we want also to have a class of values
-- from which we can extract subjects. This is called 'subject-oriented 
-- programming.'

class Subjective a where
   subjects :: a -> [Subject]

instance ToRow Subject where
   toRow subj = undefined

instance FromRow Subject where
   fromRow = undefined

type IxSubject = IxValue Subject -- ISubj { subjIdx :: Integer, subject :: String }
   -- deriving (Eq, Ord, Show)

{--
instance Indexed IxSubject where
   idx subj = undefined

instance FromRow IxSubject where
   fromRow = undefined
--}

fetchSubjectsStmt :: Query
fetchSubjectsStmt = [sql|SELECT * from subject|]

fetchSubjects :: Connection -> IO [IxSubject]
fetchSubjects conn = undefined

{--
subjects, declared below, can be simply done with tups and bifurcate

subjects :: [IxSubject] -> (Map Integer String, Map String Integer)
subjects subjs = undefined

Okay, so we can read the current state from the database. That's great!

Now let's look at a workflow.

We have a set of subjects in the database, we parse some articles with subjects,
some are already in the database, some are new subjects.

What do we do when we want to join these subjects to the articles we store into
the database?

1. we read the subjects from the database and get the mapped (index,subject)
   results (see the function subjects, above).
2. we parse the new articles and collect the subjects extracted as metadata.
3. For those subjects already stored, we have the keyed index.
4. for those not yet stored, we store those into the database, which then 
   returns their automatically generated keyed indices as a result, we create 
   those new associations, remembering that these are new subjects
5. We create the pivot table from the new map.

Okay, so that's the workflow. Let's go about doing this, creating the structure
we need for the workflow-context.

-- moved declaration to Data.MemoizingTable

data MemoizingTable a b =
   MT { fromTable :: Map a b, readIndex :: Map b a, newValues :: Set b }
      deriving Show
--}

initMemTable :: Ord a => Ord b => (Map a b, Map b a) -> MemoizingTable a b
initMemTable (keys, vals) = undefined

{--
So, to make subjects a memoizing table, we read in the subjects from the 
database. As we read in articles, we scan the readKey map for subjects already
stored into the database. Those we get the indicies. For those new subjects,
we add those to the newValues.

Let's put this into practice.
--}

data Part = ONE | TWO
   deriving (Eq, Ord)

instance Show Part where
   show ONE = "1"
   show TWO = "2"

archive :: Part -> FilePath
archive x =
   "Y2017/M10/D03/NYTOnline_09-05-17_09-10-17_ALLSecs-pt" ++ show x ++ ".txt.gz"

-- I'm thinking of taking a stately State-ful approach to updating the
-- memoizing table.

type SubjectTable = MemoizingTable Integer Subject

type MemoizingState m a = StateT (SubjectTable, Map Index [Subject]) m a

-- now we use our Subjective instances to extract our subjects

getSubjectsMT :: Subjective s => Monad m => Index -> s -> MemoizingState m ()
getSubjectsMT ix art = undefined

-- we get the article, extract its subject information then factor the subject
-- into the ones already indexed verse the ones we haven't yet stored in the
-- database. We also update the map of subjects in each article.

uploadSubjectsStmt :: Query
uploadSubjectsStmt = [sql|INSERT INTO subject (subject) VALUES (?) returning id|]

uploadSubjects :: Connection -> [Subject] -> IO [Index]
uploadSubjects conn subjs = undefined

uploadMT :: Connection -> SubjectTable -> IO [IxSubject]
uploadMT conn table = undefined

-- uploads the new subjects discovered in parsing the articles and then
-- gets back the indices for those new subjects

-- Okay, now we've got the indexed subjects, we update the MemoizingTable
-- with those new subjects:

updateMT :: [IxSubject] -> SubjectTable -> SubjectTable
updateMT subjs table = undefined

-- The updated subjects should be the set of used-to-be-new subjects in the
-- memoizing table. Clear that set and update the maps with the new information

-- Now we should have everything we need to upload the subjects associated
-- with their source articles. The subjects are all uploaded, all we have to
-- do is create the Pivot table values to match articles to subjects. We have
-- both the article indices, their associated subjects, and the subject indices
-- in the MemoizingState.

buildSubjectPivots :: Monad m => MemoizingState m [Pivot]
buildSubjectPivots = undefined

insertSubjPivotStmt :: Query
insertSubjPivotStmt =
   [sql|INSERT INTO article_subject (article_id,subject_id) VALUES (?,?)|]

insertSubjPivot :: Connection -> [Pivot] -> IO ()
insertSubjPivot conn pivots = undefined

{-- BONUS -----------------------------------------------------------------

The subject table structure is not uncommon. Create a type that has a key-value
pair and create FromRow and ToRow instances of it.

-- moving these declarations to Store.SQL.Util.Indexed

data IxValue = WhatKeyValuePair

instance ToRow IxValue where
   toRow ixv = undefined

instance FromRow IxValue where
   fromRow = undefined

-- from the function archive, parse the Part ONE articles, insert those articles
-- and the associated auxilary information, including names and subjects.

-- You can roll your own ETL process for this. Hint: see the bonus-bonus question.

-- BONUS-BONUS -----------------------------------------------------------

Yesterday's etlProcess got it done for article and name insertion. Add the
functionality of Subject insertion, AND time each command (function that
interacts with IO) in the ETL. Report the times here with a total time for
the whole ETL process on the archives here. Do this using the articles stored
in the archive function, Part TWO.
--}

timedETL :: Compressed -> Connection -> IO NominalDiffTime
timedETL archive conn = undefined
