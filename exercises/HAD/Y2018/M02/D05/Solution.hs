{-# LANGUAGE QuasiQuotes #-}

module Y2018.M02.D05.Solution where

{--
So, our log-type assumes that the time of insertion is the record of when the
log was made. Okay. But what happens if the log entry occurs seconds or longer
after the event because of external factors, such as a computation in 
exponential time, or because a prolonged interaction with The Real World via
the IO monad?

Let's enhance the LogEntry type with timestamp. Then, when we insert that
entry into the database, let's add that timestamp.
--}

import Data.Functor (void)
import Control.Monad.Writer

import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Control.DList (DList, dl')

import Data.LookupTable (LookupTable)
import Data.Logger (LogEntry, LogEntry'(LE'))

import Store.SQL.Util.Logging hiding (insertStampedEntries)

data Stamped a = Stamped { stamped :: a, time :: LocalTime }
   deriving Show

instance ToRow a => ToRow (Stamped a) where
   toRow se = toField (time se):toRow (stamped se)

-- hint: see below. I put the time-field at the head of the insert columns

stampIt :: a -> IO (Stamped a)
stampIt e =
   getCurrentTime >>= \t -> Stamped e <$> (`utcToLocalTime` t) <$> getTimeZone t

-- takes a log entry and puts the current timestamp on it

type StampedWriter a = WriterT (DList (Stamped a)) IO

-- sayIO may be an useful function to define as well:

sayIO :: a -> StampedWriter a ()
sayIO entry = lift (dl' <$> stampIt entry) >>= tell

-- ... as it 'automatically' timestamps a logentry for you as it puts it in
-- the log.

insertStampedEntryStmt :: Query
insertStampedEntryStmt =
   [sql|INSERT INTO log (time,severity,app,module,message) VALUES (?,?,?,?,?)|]

{-- moving to Store.SQL.Util.Logging:
insertStampedEntries :: Connection -> LookupTable -> [Stamped LogEntry] -> IO ()
insertStampedEntries conn lktab =

-- hint: you may wish to introduce an intermediary type to handle insertion of
-- severity-values, as managed for LogEntry values in Store.SQL.Util.Logging

-- okay, bad hint. We need to unwrap the log entries and make them LogEntry'
-- values

   void . executeMany conn insertStampedEntryStmt
        . map (\(Stamped a t) -> Stamped (LE' a lktab) t)
--}

-- The concept of Stamped will be moved to Data.Stamped and 
-- Store.SQL.Util.Stamping. The new stamped log entry type will be rolled into 
-- Store.SQL.Util.Logging
