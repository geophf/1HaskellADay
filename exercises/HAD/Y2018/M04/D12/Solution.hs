module Y2018.M04.D12.Solution where

{--
Log entries and audit entries, ... where audit entries are another kind of log
entries. Today's Haskell problem, write to the log, and write to the audit log.

The twist here is that both tables (pictured in this directory at

Y2018/M04/D12/logs-n-audit.png

have ancillary lookup tables that they use to create their entries, so you
must have those lookup tables already populated to make entries.

Lezzgo!
--}

import Prelude hiding (log)

import Database.PostgreSQL.Simple

-- the below imports are available via 1HaskellADay git repository

import Data.AuditLogger
import Data.Logger
import Data.LookupTable
import Data.Time.Stamped

import Store.SQL.Connection
import Store.SQL.Util.AuditLogging
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable

import Y2018.M04.D11.Solution

-- from yesterday's protec value, create an etl process that inserts that
-- value, logs whatever you need to log (like: "loaded packet; parsed articles;
-- stored packet"), then makes an audit entry for this application

etlPacket :: Connection -> Protec -> IO ()
etlPacket conn prot =
   insertProtec conn prot                                >>= \pidx ->
         -- we'll use this packetIndex when we insert articles
   lookupTable conn "severity_lk"                        >>= \sev ->
   log ("Insert packet " ++ show prot)                   >>=
   insertStampedEntries conn sev . pure                  >>
   lookupTable conn "active_lk"                          >>= \actv ->
   lookupTable conn "action_lk"                          >>= \actn ->
   mapM_ (storeAuditInfo conn actv actn (show (succ (page prot))))
                  (zipWith IxV (map idx pidx) [prot])

log :: String -> IO (Stamped LogEntry)
log = stampIt . Entry INFO "etlPacket" "Y2018.M04.D12.Solution"

-- also remember you need to have extracted the populated lookup tables
-- (severity, active, and action) from the database as Haskell LookupTable
-- values

-- What does your database look like after the packet insert?

{--
>>> withConnection WPJ (`etlPacket` protec)

New rows insert into log, audit and packet
--}
