{-# LANGUAGE QuasiQuotes #-}

module Y2018.M02.D09.Exercise where

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

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Control.Presentation (Univ)
import Control.Scan.CSV (uncsv)

import Data.Logger (LogEntry)
import Data.Stamped (Stamped)

import Store.SQL.Util.Indexed (IxValue)

-- read in the out-dated data.

-- so, for example: logging

instance FromRow LogEntry where
   fromRow = undefined

-- with that instance we can create a IxValue (Stamped LogEntry) set

fetchLogsStmt :: Query
fetchLogsStmt = [sql|SELECT * FROM log|]

-- this is one way to do it: grab it all and sort out what we care about
-- in Haskell. The other is to discriminate in the SQL query, adding a WHERE
-- clause that returns only the out-dated rows.

type Row a = IxValue (Stamped a)

fetchLogs :: Connection -> IO [Row LogEntry]
fetchLogs conn = undefined

-- Now, partition the returned log entries into ones we want to keep in the
-- database and the ones we want to save off to file for cold storage.

partitionLogs :: [Row LogEntry] -> ([Row LogEntry], [Row LogEntry])
partitionLogs rows = undefined

-- Now save off the old rows

saveOldRows :: Univ a => FilePath -> [a] -> IO ()
saveOldRows toFile rows = undefined

-- Now delete those old rows from the database

deleteOldRowsStmt :: Query
deleteOldRowsStmt = [sql|DELETE FROM log WHERE id IN (?)|]

deleteOldRows :: Connection -> [Row LogEntry] -> IO ()
deleteOldRows conn rows = undefined

-- Now imbibe a celebratory beverage of your choice. YAY!
