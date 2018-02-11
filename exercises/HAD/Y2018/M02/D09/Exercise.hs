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

import Data.Time
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Control.Presentation (Univ, explode)
import Control.Scan.CSV (uncsv)

import Data.Logger (LogEntry)
import Data.LookupTable (LookDown)
import Data.Stamped (Stamped)

import Store.SQL.Util.Indexed (IxValue)

-- read in the out-dated data.

-- so, for example: logging

instance FromRow LogEntry where
   fromRow = undefined

-- with that instance we can create a IxValue (Stamped LogEntry) set

fetchLogsStmt :: Query
fetchLogsStmt =
   [sql|SELECT id,time,severity,app,module,message FROM log WHERE time < ?|]

-- this is one way to do it: grab it all and sort out what we care about
-- in Haskell. The other is to discriminate in the SQL query, adding a WHERE
-- clause that returns only the out-dated rows.

type Row a = IxValue (Stamped a)

fetchLogs :: Connection -> LookDown -> Day -> IO [Row LogEntry]
fetchLogs conn sev day = undefined

-- Now save off the old rows

saveOldRows :: Univ a => FilePath -> [a] -> IO ()
saveOldRows toFile rows = undefined

-- of course, to save the rows, we need to make the LogEntry a Univ instance

instance Univ LogEntry where
   explode log = undefined

-- and the stamped type univ, too:

instance Univ a => Univ (Stamped a) where
   explode stamped = undefined

-- and the indexed type a univ:

instance Univ a => Univ (IxValue a) where
   explode ix = undefined

-- Now delete those old rows from the database

deleteOldRowsStmt :: Integer -> String
deleteOldRowsStmt = (" DELETE FROM log WHERE id=" ++) . (++ ";") . show

bufferedDeleteOldRows :: Connection -> [Row LogEntry] -> IO ()
bufferedDeleteOldRows conn rows = undefined

{-- BONUS -----------------------------------------------------------------

Tie this all together to create an application that saves off the old log 
entries to some file and the deletes those saved-off entries from the log table.
--}

main' :: [String] -> IO ()
main' [file] = undefined    -- saved old log entries to file
main' _ = undefined         -- shows usage message

-- Now imbibe a celebratory beverage of your choice. YAY!
