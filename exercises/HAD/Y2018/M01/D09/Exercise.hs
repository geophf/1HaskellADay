{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D09.Exercise where

{--
Okay, we've parsed articles from JSON and we've stored those articles.

Let's start expanding the scope here both in breath and in depth.

Depth first.

What happens when we don't parse an article? Or we can't store one?

We've logged parsing process information to a Logger-type (of the Writer monad),
today, let's record what we've logged into the database with a new log-table.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.Logger

import Y2018.M01.D02.Exercise hiding (etl)

data Severity = TRACE | DEBUG | INFO | WARN | ERROR | FATAL
   deriving (Eq, Ord, Show)

instance ToField Severity where
   toField s = undefined

data LogEntry = Entry { sev :: Severity,app,mod,msg :: String }
   deriving (Eq, Show)

instance ToRow LogEntry where
   toRow entr = undefined

insertLogEntryStmt :: Query
insertLogEntryStmt =
   [sql|INSERT INTO log (severity,app,module,message)
        VALUES ((SELECT id FROM severity_lk WHERE level=?),?,?,?)|]

insertLogEntries :: Connection -> [LogEntry] -> IO ()
insertLogEntries conn entries = undefined

-- modify the ETL process from Y2018.M01.D02.Exercise to spill the log entries
-- to the database (also, the Logger m should be cleared, so you don't keep
-- reentering them.
