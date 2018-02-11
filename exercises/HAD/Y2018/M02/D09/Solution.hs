{-# LANGUAGE QuasiQuotes #-}

module Y2018.M02.D09.Solution where

{--
Along the way we've been accumulating data. That's what you do in data stores,
accumulate data. But what happens when data ages out and is no longer useful?

When, it's not going to ride off into the sunset itself. You have to do that
cleanup yoursef.

Today's Haskell problem is a two-parter:

1. read in the out-dated rows of data you want to remove from the database
   into Haskell values.

2. Save those rows of data to file.

3. delete the rows of out-dated data.

Two parts. As I said.
--}

import Codec.Compression.GZip as GZ

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (maximumBy, partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Control.List (list)
import Control.Presentation (Univ, explode)
import Control.Scan.CSV (uncsv)

import Data.LookupTable (LookDown, lookdown)
import Data.Logger (LogEntry(Entry), Severity(INFO))
import Data.Stamped (Stamped(Stamped), time, stampIt)

import Store.SQL.Connection (withConnection, connectInfo)
import Store.SQL.Util.Indexed (IxValue(IxV), val, idx, Index(Idx))
import Store.SQL.Util.Logging (insertStampedEntries)
import Store.SQL.Util.LookupTable (lookupTable)

import Y2018.M01.D29.Solution (oneWeekAgo)

-- read in the out-dated data.

-- so, for example: logging

{--
The complication of reading in the log table is that we have meaningful
enumerations on the severity-level, whereas to get that in PostGreSQL query
we need to join in the severity_lk for that information. Well, will this
work? Let's attack in in stagese.

Stage 1: the LogEntry type
--}

-- with that instance we can create a IxValue (Stamped LogEntry) set

fetchLogStmt :: Query
fetchLogStmt =
   [sql|SELECT id,time,severity,app,module,message FROM log WHERE time < ?|]

-- this is one way to do it: grab it all and sort out what we care about
-- in Haskell. The other is to discriminate in the SQL query, adding a WHERE
-- clause that returns only the out-dated rows.

type Row a = IxValue (Stamped a)

type LogTup = (Integer,LocalTime,Integer,String,String,String)

fetchLogs :: Connection -> LookDown -> Day -> IO [Row LogEntry]
fetchLogs conn lk day = map (row2log lk) <$> query conn fetchLogStmt (Only day)

-- Now we need to convert from a 6-tuple to a IxValue (Stamped LogEntry)

row2log :: LookDown -> LogTup -> Row LogEntry
row2log sev_lk (i,t,sev,a,m,msg) =
   IxV i (Stamped (Entry (read (sev_lk Map.! sev)) a m msg) t)

{--
>>> connectInfo
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> sev <- lookdown <$> lookupTable conn "severity_lk"
>>> sev
fromList [(1,"TRACE"),(2,"DEBUG"),(3,"INFO"),(4,"WARN"),(5,"ERROR"),(6,"FATAL")]
>>> owa <- oneWeekAgo conn
>>> owa
2018-02-03
>>> log <- fetchLogs conn sev owa
>>> close conn
>>> length log
275513
>>> (head &&& last) log
(IxV {ix = 1, val = Stamped {stamped = Entry {sev = INFO, app = "etl_pilot",
      mod = "Y2018.M01.D10.Solution", msg = "Inserted Pack..." },
      time = 2018-01-13 07:37:11.403969}},
 IxV {ix = 275513, val = Stamped {stamped = Entry {sev = INFO, 
      app = "daily upload", mod = "Y2018.M01.D31.Solution", 
      msg = "Inserted Pack ..."}, time = 2018-01-13 07:37:11.403969}})
--}

{-- Now, partition the returned log entries into ones we want to keep in the
-- database and the ones we want to save off to file for cold storage.

partitionLogs :: [Row LogEntry] -> ([Row LogEntry], [Row LogEntry])
partitionLogs rows =
   let dayo = time (val (maximumBy (compare `on` time . val) rows))
       lastweek = dayo { localDay = addDays (-7) (localDay dayo) } in
   partition ((< lastweek) . time . val) rows

Partitioning the logs is no longer necessary as we do that in the SQL query.
--}

-- Now save off the old rows

saveOldRows :: Univ a => FilePath -> [a] -> IO ()
saveOldRows file =
   BL.writeFile file . GZ.compress . BL.pack . unlines . map uncsv

-- of course, to save the rows, we need to make the LogEntry a Univ instance

instance Univ LogEntry where
   explode (Entry sev app mod msg) = show sev:[app, mod, msg]

-- and the stamped type univ, too:

instance Univ a => Univ (Stamped a) where
   explode (Stamped s t) = show t:explode s

-- and the indexed type a univ:

instance Univ a => Univ (IxValue a) where
   explode (IxV ix val) = show ix:explode val

-- Now delete those old rows from the database

deleteOldRowsStmt :: Integer -> String
deleteOldRowsStmt = (" DELETE FROM log WHERE id=" ++) . (++ ";") . show

bufferedDeleteOldRows :: Connection -> [IxValue a] -> IO ()
bufferedDeleteOldRows conn [] = return ()
bufferedDeleteOldRows conn rows@(_:_) =
   let (r,ows) = splitAt 10000 rows in
   execute_ conn (Query . B.pack $ concatMap (deleteOldRowsStmt . idx) r)   >>
   putStrLn ("Deleted 10,000 rows starting at idx " ++ show (idx (head r))) >>
   bufferedDeleteOldRows conn ows

{-- BONUS -----------------------------------------------------------------

Tie this all together to create an application that saves off the old log
entries to some file and the deletes those saved-off entries from the log table.
--}

main' :: [String] -> IO ()
main' [archive] =
   putStrLn ("clean_log to " ++ archive)                             >>
   withConnection (\conn ->
      lookupTable conn "severity_lk"                                 >>= \sev ->
      oneWeekAgo conn                                                >>= \owa ->
      fetchLogs conn (lookdown sev) owa                              >>=
      list (putStrLn "No log entries to archive today.")
           (\log -> putStrLn "Fetched logs."                              >>
                    let file = archive ++ ('-':show owa ++ ".txt.gz") in
                    saveOldRows file log                                  >>
                    putStrLn ("Saved logs to " ++ file)                   >>
                    bufferedDeleteOldRows conn log                        >>
                    putStrLn "Deleted logs."                              >>
                    let [count, min, max] = minimaxcount log 
                        msg = "Archived " ++ count ++ " log entries (" ++ min
                           ++ ", " ++ max ++ ") to " ++ file in
                    stampIt (Entry INFO "clean_logs"
                                   "Y2018.M02.D09.Solution" msg)          >>=
                    insertStampedEntries conn sev . pure                  >>
                    putStrLn msg))
main' _ = putStrLn (unlines (["", "archive_logs <to_archive>", "",
   "\tTake log entries older than a week and saves them ",
   "\tto <to_archive>-<date>.txt.gz, then deletes those log entries from the ",
   "\tlog data table.", ""]))

minimaxcount :: [IxValue a] -> [String]
minimaxcount (IxV i _:r) =
   map show (foldr (\(IxV i _) [c,n,x] -> [c+1,min n i,max x i]) [0,i,i] r)

{--
>>> minimaxcount log
["275512","1","275513"]

>>> main' ["pilot-logs"]
clean_log to pilot-logs
Fetched logs.
Saved logs to pilot-logs-2018-02-03.txt.gz
Deleted 10,000 rows starting at idx 1
Deleted 10,000 rows starting at idx 10001
Deleted 10,000 rows starting at idx 20001
Deleted 10,000 rows starting at idx 30001
Deleted 10,000 rows starting at idx 40001
Deleted 10,000 rows starting at idx 50001
Deleted 10,000 rows starting at idx 60001
Deleted 10,000 rows starting at idx 70001
Deleted 10,000 rows starting at idx 80001
Deleted 10,000 rows starting at idx 90001
Deleted 10,000 rows starting at idx 100001
Deleted 10,000 rows starting at idx 110001
Deleted 10,000 rows starting at idx 120001
Deleted 10,000 rows starting at idx 130001
Deleted 10,000 rows starting at idx 140001
Deleted 10,000 rows starting at idx 150001
Deleted 10,000 rows starting at idx 160001
Deleted 10,000 rows starting at idx 170001
Deleted 10,000 rows starting at idx 180001
Deleted 10,000 rows starting at idx 190001
Deleted 10,000 rows starting at idx 200001
Deleted 10,000 rows starting at idx 210001
Deleted 10,000 rows starting at idx 220001
Deleted 10,000 rows starting at idx 230000
Deleted 10,000 rows starting at idx 240000
Deleted 10,000 rows starting at idx 250000
Deleted 10,000 rows starting at idx 260001
Deleted 10,000 rows starting at idx 270001
Deleted logs.
Archived 275512 log entries (1, 275513) to pilot-logs-2018-02-03.txt.gz

$ ls -l pilot-logs*.gz
-rw-r--r--  1 geophf  staff  7099874 Feb 10 21:43 pilot-logs-2018-02-03.txt.gz

AND THEN, we do it again to test the 'no log entries'-case:

>>> main' ["p-prime"]
clean_log to p-prime
No log entries to archive today.
--}

-- Now imbibe a celebratory beverage of your choice. YAY!
