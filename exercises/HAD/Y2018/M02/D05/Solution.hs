{-# LANGUAGE QuasiQuotes #-}

module Y2018.M02.D05.Exercise where

{--
So, our log-type assumes that the time of insertion is the record of when the
log was made. Okay. But what happens if the log entry occurs seconds or longer
after the event because of external factors, such as a computation in 
exponential time, or because a prolonged interaction with The Real World via
the IO monad?

Let's enhance the LogEntry type with timestamp. Then, when we insert that
entry into the database, let's add that timestamp.
--}

import Control.Monad.Writer

import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Control.DList (DList)

import Data.LookupTable (LookupTable)
import Data.Logger (LogEntry)

import Store.SQL.Util.Logging

data Stamped a = Stamped { stamped :: a, time :: LocalTime }
   deriving Show

instance ToRow a => ToRow (Stamped a) where
   toRow se = undefined

-- hint: see below. I put the time-field at the head of the insert columns

stampIt :: a -> IO (Stamped a)
stampIt entry = undefined

-- takes a log entry and puts the current timestamp on it

type StampedLogger = WriterT (DList (Stamped LogEntry)) IO

-- sayIO may be an useful function to define as well:

sayIO :: LogEntry -> StampedLogger ()
sayIO entry = undefined

-- ... as it 'automatically' timestamps a logentry for you as it puts it in
-- the log.

insertStampedEntryStmt :: Query
insertStampedEntryStmt =
   [sql|INSERT INTO log (time,severity,app,module,message) VALUES (?,?,?,?,?)|]

insertStampedEntries :: Connection -> LookupTable -> [Stamped LogEntry] -> IO ()
insertStampedEntries conn lktab entries = undefined

-- hint: you may wish to introduce an intermediary type to handle insertion of
-- severity-values, as managed for LogEntry values in Store.SQL.Util.Logging

-- Once defined, the concept of Stamped will be moved to Store.SQL.Util.Stamped

-- The new stamped log entry type will be rolled into Store.SQL.Util.Logging
