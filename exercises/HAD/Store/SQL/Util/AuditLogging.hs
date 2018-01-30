{-# LANGUAGE QuasiQuotes #-}

module Store.SQL.Util.AuditLogging where

{--
Yesterday we looked at recording log entries.

Today we'll look at recording auditing information.

What is auditing information?

When an application runs we'd kind of like to know that it did run (to 
completion or part-way and successfully or not), when it ran, and what's the 
last piece of data the application processed.

Now, ETL and auditing go hand in glove, but there are other applications that
either do things incrementally or give a report of statistics or progress as
part of their process. These may fit in a log, but may be a better fit in the
audit log.

So, here we go: when the ETL processes a chunk of records, record the last
article successfully processed along with any (meta-)data associated with
that article that will allow us to pick up where we left off.
--}

import Control.Monad (void)

import qualified Data.Map as Map
import Data.Time
import Data.Time.Clock

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField hiding (Action)
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.AuditLogger
import Data.LookupTable

import Store.SQL.Util.Indexed

-- we create the ToRow instance of an audit log entry:

instance ToRow AuditEntry where
   toRow (AE app user tab col delt row act time) =
      toField row:toField time:map toField [app, user, tab, delt]

-- and the field representation of an action (using the action_lk table)

instance ToField Action' where
   toField (Act' act lk) = toField (lk Map.! show act)

-- so and AuditEntry' is the indexed action value:

instance ToRow AuditEntry' where
   toRow (AE' ent lk) = toRow ent ++ [toField (aud2act' ent lk)]

-- given, of course, you have a mapping from Action -> Action'

aud2act' :: AuditEntry -> LookupTable -> Action'
aud2act' (AE _ _ _ _ _ _ a _) = Act' a

{--
Great, we have the preliminaries out of the way.

make an entry in the audit table, given the next-value of a Packet and the id
of the article table of the last article inserted, with an audit entry with
these values:

app: "ETL"
user: "SYSTEM"
table_name: "article"
row_number: article id column value
action: INSERT
change: show next packet value
--}

mkAuditStmt :: String -> IxValue article -> IO AuditEntry
mkAuditStmt nextPac article =
   getCurrentTime >>= \utc ->
   getCurrentTimeZone >>= \tz ->
   return (AE "ETL" "SYSTEM" "article" Nothing nextPac
              (idx article) INSERT (utcToLocalTime tz utc))

-- then insert that statement into the database

insertAuditEntryStmt :: Query
insertAuditEntryStmt =
   [sql|INSERT INTO audit (row_number,time,application,user_name,table_name,change,action)
        VALUES (?,?,?,?,?,?,?)|]

insertAuditEntries :: Connection -> LookupTable -> String -> IxValue a -> IO ()
insertAuditEntries conn lk nextpac ix =
   mkAuditStmt nextpac ix >>=
   void . executeMany conn insertAuditEntryStmt . map (`AE'` lk) . pure

-- BUT ONLY AFTER deactivating all previously inserted audit records

deactivateAuditEntriesStmt :: Query
deactivateAuditEntriesStmt = [sql|UPDATE audit SET active_ind=?|]

deactivatePriorAuditEntries :: Connection -> LookupTable -> IO ()
deactivatePriorAuditEntries conn lk =
   void (execute conn deactivateAuditEntriesStmt (Only (lk Map.! show INACTIVE)))

-- and execute that before doing the insert, thus:

storeAuditInfo :: Connection -> LookupTable -> LookupTable -> String
               -> IxValue a -> IO ()
storeAuditInfo conn activ actn nextPack art =
   deactivatePriorAuditEntries conn activ >>
   insertAuditEntries conn actn nextPack art

{-- BONUS -----------------------------------------------------------------

Rewrite the ETL to insert an audit entry for the packet uploaded.

example of audit logging:

etl :: BlockParser Identity Authors
    -> (Connection -> [IxValue (DatedArticle Authors)] -> IO ())
    -> Connection -> FilePath -> IO ()
etl generator ancillaryFn conn json =
   lookupTable conn "active_lk"                 >>= \actv ->
   lookupTable conn "action_lk"                 >>= \actn ->
   lookupTable conn "severity_lk"               >>= \sev  ->
   readStorePacket conn json                    >>= \pac  ->
   gruntWerk sev generator ancillaryFn conn pac >>=
   storeAuditInfo conn actv actn pac
--}
