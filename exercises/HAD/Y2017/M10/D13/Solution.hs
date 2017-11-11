module Y2017.M10.D13.Solution where

{--
Today, for something COMPLETELY different, ... yet UTTERLY the same ...

We're going to build and then deploy an application.

Take the ETL process you've built over the ... is it a month now? IT'S A MONTH!
... last month and create an application. Run it on the 'command line' or the
shell or as a web service. Use the below referenced filepaths as compressed
archived and load these data sets up to your PostgreSQL data store.

LOOK AT YOU! PRODUCTION READY!

But your application needs a name! What shall you call your app?

Well, obviously: Y2017.M10.D13.Solution, of course!

Let's do this.
--}

import Control.Arrow (second, (&&&))
import Control.Monad (zipWithM_)
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Time
import Data.Time.Clock
import Database.PostgreSQL.Simple
import System.Environment

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as Mem
import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts (inserter)
import Store.SQL.Util.Pivots (joinValue)

import Y2017.M09.D25.Solution (Article, title, metadata)
import Y2017.M09.D28.Solution (insertArts)
import Y2017.M09.D29.Solution (raw2persons, insertPers, insertArtPersJoinStmt)
import Y2017.M10.D02.Solution

import Y2017.M10.D03.Solution     -- subject memoization

main' :: [String] -> IO ()
main' files@(_:_) = withConnection (flip mapM_ files . runETL)
main' [] = putStrLn (unlines ["", "etl <compressed archives>", "",
   "\tscans compressed archive and stores in database pointed to in "
       ++ "environment"])

-- main takes a filepath argument, reads in that compressed archive and stores
-- the reified result into the PostgreSQL database.

runETL :: Connection -> String -> IO ()
runETL conn archive =
   getCurrentTime                                >>= \start -> 
   putStrLn "ETL process: start."                >>
   extractBlocks <$> BL.readFile archive         >>=
   transformLoad conn                            >>
   getCurrentTime                                >>=
   putStrLn . ("ETL process complete: " ++)
            . show . flip diffUTCTime start

-- say we need to modify the blocks, e.g.: to replace special characters with
-- ASCII-equivalents:

transformLoad :: Connection -> [Block] -> IO ()
transformLoad conn blocks =
   insertBlocks conn blocks >>= \ixblks ->
   let articles = join (zipWith block2Article ixblks blocks) in
   insertArts conn articles >>= \ixarts ->
   let rns = catMaybes (zipWith art2RawNames ixarts articles)
       pers = concatMap raw2persons rns in
   insertPers conn pers >>= \ixpers ->
   inserter conn insertArtPersJoinStmt (zipWith joinValue pers ixpers) >>
   fetchSubjects conn >>= \isubs ->
   let memtable = Mem.start (map ixsubj2pair isubs)
       stat = execState (zipWithM_ getSubjectsMT ixarts articles)
                                   (memtable, Map.empty) in
   uploadMT conn (fst stat) >>= \ixsubs ->
   let tab = Mem.update (map ixsubj2pair ixsubs) (fst stat)
       pivs = evalState buildSubjectPivots (tab, snd stat)
       subjs = mt2IxSubjects tab in
   insertSubjPivot conn pivs

mt2IxSubjects :: MemoizingTable Integer Subject -> [IxValue String]
mt2IxSubjects = map (uncurry IxV . second subj) . Map.toList . Mem.fromTable

dir :: FilePath
dir = "Y2017/M10/D13/"

sources :: FilePath -> [String] -> [FilePath]
sources dir = map (((dir ++ nyt) ++) . (++ ".txt.gz"))
   where nyt = "NYTOnline_08-29-17_09-05-17_pt"

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it

I separated out runETL so I can sequence ETL operations with one connection:

>>> mapM_ (runETL conn) (sources dir $ words "1 2")

ETL process: start.
ETL process complete: 2.379852s
ETL process: start.
ETL process complete: 2.699108s

>>> close conn

AUTOMATION!
--}
