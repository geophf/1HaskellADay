module Y2018.M04.D12.Exercise where

{--
Log entries and audit entries, ... where audit entries are another kind of log
entries. Today's Haskell problem, write to the log, and write to the audit log.

The twist here is that both tables (pictured in this directory at

Y2018/M04/D12/logs-n-audit.png

have ancillary lookup tables that they use to create their entries, so you
must have those lookup tables already populated to make entries.

Lezzgo!
--}

import Database.PostgreSQL.Simple

-- the below imports are available via 1HaskellADay git repository

import Data.AuditLogger
import Data.Logger
import Data.LookupTable

import Store.SQL.Connection
import Store.SQL.Util.AuditLogging
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable

import Y2018.M04.D11.Exercise

-- from yesterday's protec value, create an etl process that inserts that
-- value, logs whatever you need to log (like: "loaded packet; parsed articles;
-- stored packet"), then makes an audit entry for this application

etlPacket :: Connection -> Protec -> IO ()
etlPacket conn prot = undefined

-- also remember you need to have extracted the populated lookup tables
-- (severity, active, and action) from the database as Haskell LookupTable
-- values

-- What does your database look like after the packet insert?
