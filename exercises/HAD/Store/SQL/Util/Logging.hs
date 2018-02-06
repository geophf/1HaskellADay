{-# LANGUAGE QuasiQuotes #-}

module Store.SQL.Util.Logging where

-- stores log entries in a SQL log table

import Data.Functor (void)

import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow

-- below import available via 1HaskellADay git repository

import Data.Logger
import Data.LookupTable (LookupTable)
import Data.Stamped

import Store.SQL.Util.Stamping -- for ToRow instance

instance ToRow LogEntry where
   toRow (Entry _ a m e) = map toField [a,m,e]

instance ToRow LogEntry' where
   toRow (LE' ent lk) = 
      toField (lk Map.! show (sev ent)):toRow ent

insertLogEntryStmt :: Query
insertLogEntryStmt = 
   [sql|INSERT INTO log (severity,app,module,message) VALUES (?,?,?,?)|]

insertLogEntries :: Connection -> LookupTable -> [LogEntry] -> IO ()
insertLogEntries conn lk =
   void . executeMany conn insertLogEntryStmt . map (`LE'` lk)

{-- 
So, our log-type assumes that the time of insertion is the record of when the
log was made. Okay. But what happens if the log entry occurs seconds or longer
after the event because of external factors, such as a computation in 
exponential time, or because a prolonged interaction with The Real World via
the IO monad?

Let's enhance the LogEntry type with timestamp. Then, when we insert that
entry into the database, let's add that timestamp.
--}

insertStampedEntryStmt :: Query
insertStampedEntryStmt =
   [sql|INSERT INTO log (time,severity,app,module,message) VALUES (?,?,?,?,?)|]

insertStampedEntries :: Connection -> LookupTable -> [Stamped LogEntry] -> IO ()
insertStampedEntries conn lktab =
   void . executeMany conn insertStampedEntryStmt
        . map (\(Stamped a t) -> Stamped (LE' a lktab) t)
