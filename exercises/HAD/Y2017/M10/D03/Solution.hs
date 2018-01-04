{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Y2017.M10.D03.Solution where

{--
Today we're going to look at the MemoizingTable-type.

What is the MemoizingTable-type?

Well, it's what we're going to look at today!

(circular-reference much, geophf?
me: what?)
--}

import Control.Arrow ((&&&), (>>>), (***), second)
import Control.Monad (void, (<=<))
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import Control.Scan.CSV (rend)
import Data.MemoizingTable (MemoizingTable(MT))
import qualified Data.MemoizingTable as MT
import Store.SQL.Connection (connectInfo)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts
import Store.SQL.Util.Pivots

import Y2017.M09.D25.Solution (Article, metadata)
import Y2017.M09.D28.Solution (insertArts)
import Y2017.M09.D29.Solution (raw2persons, insertPers, insertArtPersJoinStmt)
import Y2017.M10.D02.Solution

{--
The MemoizingTable type addresses this problem: you have a set of words in a
data table:
--}

data Subject = Subj { subj :: String }
   deriving (Eq, Ord, Show)

instance ToRow Subject where
   toRow = pure . toField . subj

instance FromRow Subject where
   fromRow = Subj <$> field

type IxSubject = IxValue Subject -- ISubj { subjIdx :: Integer, subject :: String }
   -- deriving (Eq, Ord, Show)

{--
instance Indexed IxSubject where
   idx = subjIdx

instance FromRow IxSubject where
   fromRow = ISubj <$> field <*> field
--}

-- So, since we have a Subject-type, we want also to have a class of values
-- from which we can extract subjects. This is called 'subject-oriented
-- programming.'

class Subjective a where
   subjects :: a -> [Subject]

fetchSubjectsStmt :: Query
fetchSubjectsStmt = [sql|SELECT * from subject|]

fetchSubjects :: Connection -> IO [IxSubject]
fetchSubjects = flip query_ fetchSubjectsStmt

{--

-- subjects is now Data.MemoizingTable.bifurcate

subjects :: [IxSubject] -> (Map Integer String, Map String Integer)
subjects = -- the two Map.fromList functions are DIFFERENT FUNCTIONS! smh
   Map.fromList . map (idx &&& subject) &&& Map.fromList . map (subject &&& idx)

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

-- below code moved to Data.MemoizingTable

data MemoizingTable a b =
   MT { fromTable :: Map a b, readIndex :: Map b a, newValues :: Set b }
      deriving Show

initMemTable :: Ord a => Ord b => (Map a b, Map b a) -> MemoizingTable a b
initMemTable = flip (uncurry MT) Set.empty

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

type MemoizingState m a = StateT (SubjectTable, Map Integer [Subject]) m a

art2Subj :: Article -> [Subject]
art2Subj = parseSubjects <=< maybeToList . Map.lookup "Subject" . metadata

parseSubjects :: String -> [Subject]
parseSubjects = map Subj . (head &&& map tail . tail >>> uncurry (:)) . rend ';'

updateNewSubjs :: [Subject] -> SubjectTable -> SubjectTable

-- updateNewSubjs reduces to Data.MemoizingTable.triage

updateNewSubjs = flip (foldl (flip MT.triage))

-- we get the article, extract its subject information then factor the subject
-- into the ones already indexed verse the ones we haven't yet stored in the
-- database. We also update the map of subjects in each article.

uploadSubjectStmt :: Query
uploadSubjectStmt = [sql|INSERT INTO keyword_pub (keyword) VALUES (?) returning id|]

uploadSubjects :: Connection -> [Subject] -> IO [Index]
uploadSubjects = flip returning uploadSubjectStmt

uploadMT :: Connection -> SubjectTable -> IO [IxSubject]
uploadMT conn (MT _ _ news) =
   let subjs = Set.toList news in
   zipWith (flip IxV) subjs . map idx <$> uploadSubjects conn subjs

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> blocks <- extractBlocks <$> BL.readFile (archive ONE)
>>> ixblks <- insertBlocks conn blocks
>>> let articles = join (zipWith block2Article ixblks blocks)
>>> ixarts <- insertArts conn articles
>>> let rns = catMaybes (zipWith art2RawNames ixarts articles)
>>> let pers = concatMap raw2persons rns
>>> ixpers <- insertPers conn pers
>>> inserter conn insertArtPersJoinStmt (zipWith joinValue pers ixpers)
>>> let memtable = initMemTable (Map.empty , Map.empty)
>>> let stat = execState (zipWithM_ getSubjectsMT ixarts articles) (memtable, Map.empty)

>>> stat
(MT {fromTable = fromList [], readIndex = fromList [],
     newValues = fromList [Subj {subj = "Absenteeism"},Subj {subj = "Affluence"},

>>> ixsubs <- uploadMT conn (fst stat)
>>> length ixsubs
61
>>> head ixsubs
ISubj {subjIdx = 1, subject = "Absenteeism"}

$ select * from subject LIMIT 10;


id	subject
------------------------
1	Absenteeism
2	Affluence
3	Aliens
4	American history
5	Anemia
6	Audiences
7	Aviation
8	Bills
9	Books
10	Burnout

-- uploads the new subjects discovered in parsing the articles and then
-- gets back the indices for those new subjects

-- Okay, now we've got the indexed subjects, we update the MemoizingTable
-- with those new subjects:

-- The updated subjects should be the set of used-to-be-new subjects in the
-- memoizing table. Clear that set and update the maps with the new information

>>> let tab = MT.update ixsubs (fst stat)
--}

-- Now we should have everything we need to upload the subjects associated
-- with their source articles. The subjects are all uploaded, all we have to
-- do is create the Pivot table values to match articles to subjects. We have
-- both the article indices, their associated subjects, and the subject indices
-- in the MemoizingState.

buildSubjectPivots :: Monad m => MemoizingState m [Pivot]
buildSubjectPivots = get >>= \(MT _ keys _, joins) ->
   return (map (uncurry Pvt)
               (concatMap (sequence . (second (map (keys Map.!))))
                          (Map.toList joins)))

insertSubjPivotStmt :: Query
insertSubjPivotStmt =
   [sql|INSERT INTO article_kw_pub (article_id,keyword_id) VALUES (?,?)|]

insertSubjPivot :: Connection -> [Pivot] -> IO ()
insertSubjPivot = flip inserter insertSubjPivotStmt

{--
>>> insertSubjPivot conn (evalState buildSubjectPivots (tab, snd stat))
()

$ select * from article_subject LIMIT 10;

id	article_id	subject_id
1	1		7
2	1		51
3	2		58
4	2		47
5	3		36
6	3		14
7	3		56
8	3		6
9	3		35
10	3		31
--}

{-- BONUS -----------------------------------------------------------------

The subject table structure is not uncommon. Create a type that has a key-value
pair and create FromRow and ToRow instances of it.

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

instance Subjective Article where subjects = art2Subj

timedETL :: FilePath -> Connection -> IO NominalDiffTime
timedETL archive conn =
   getCurrentTime >>= \start ->
   extractBlocks <$> BL.readFile archive >>= \blocks ->
   getCurrentTime >>= \exblks ->
   putStrLn ("Extracting blocks: " ++ show (diffUTCTime exblks start)) >>
   insertBlocks conn blocks >>= \ixblks ->
   getCurrentTime >>= \inblks ->
   putStrLn ("Inserting blocks: " ++ show (diffUTCTime inblks exblks)) >>
   let articles = join (zipWith block2Article ixblks blocks) in
   insertArts conn articles >>= \ixarts ->
   getCurrentTime >>= \inarts ->
   putStrLn ("Inserting articles: " ++ show (diffUTCTime inarts inblks)) >>
   let rns = catMaybes (zipWith art2RawNames ixarts articles)
       pers = concatMap raw2persons rns in
   insertPers conn pers >>= \ixpers ->
   getCurrentTime >>= \inpers ->
   putStrLn ("Inserting names: " ++ show (diffUTCTime inpers inarts)) >>
   inserter conn insertArtPersJoinStmt (zipWith joinValue pers ixpers) >>
   getCurrentTime >>= \inpersjoin ->
   putStrLn ("Inserting name-joins: " ++ show (diffUTCTime inpersjoin inpers)) >>
   let memtable = MT.start []
       stat = execState (zipWithM_ MT.triageM (map idx ixarts) (map subjects articles))
                                   (memtable, Map.empty) in
   uploadMT conn (fst stat) >>= \ixsubs ->
   getCurrentTime >>= \newsubs ->
   putStrLn ("Inserting new subjects: " ++ show (diffUTCTime newsubs inpersjoin)) >>
   let tab = MT.update (map ix2tup ixsubs) (fst stat) in
   insertSubjPivot conn (evalState buildSubjectPivots (tab, snd stat)) >>
   getCurrentTime >>= \thatsIt ->
   let totalTime = diffUTCTime thatsIt start in
   putStrLn ("Total time: " ++ show totalTime) >>
   return totalTime

{--
ixsubj2pair :: IxSubject -> (Integer, Subject)
ixsubj2pair = idx &&& Subj . val

>>> timedETL (archive ONE) conn
"Extracting blocks: 0.000989s"
"Inserting blocks: 2.956464s"
"Inserting articles: 0.429825s"
"Inserting names: 0.053812s"
"Inserting name-joins: 0.044045s"
"Inserting new subjects: 0.052927s"
"Total time: 3.594488s"
3.594488s

>>> timedETL (archive TWO) conn
"Extracting blocks: 0.000971s"
"Inserting blocks: 2.483572s"
"Inserting articles: 1.089283s"
"Inserting names: 0.018167s"
"Inserting name-joins: 0.010377s"
"Inserting new subjects: 0.028442s"
"Total time: 3.650243s"
3.650243s

The major time was spent establishing the connection to the database, the
rest of the processes ran well enough.
--}
