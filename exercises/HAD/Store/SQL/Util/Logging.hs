{-# LANGUAGE QuasiQuotes #-}

module Store.SQL.Util.Logging where

-- stores log entries in a SQL log table

import Control.Monad (void)

import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow

-- below import available via 1HaskellADay git repository

import Data.Logger
import Data.LookupTable (LookupTable)

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
