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

-- below imports available via 1HaskellADay git repository

import Data.Logger

import Y2018.M01.D02.Exercise hiding (etl)

insertLogEntryStmt :: Query
insertLogEntryStmt = [sql|INSERT INTO log (entry) VALUES (?)|]

insertLogEntries :: Connection -> [String] -> IO ()
insertLogEntries conn entries = undefined

-- modify the ETL process from Y2018.M01.D02.Exercise to spill the log entries
-- to the database (also, the Logger m should be cleared, so you don't keep
-- reentering them.
