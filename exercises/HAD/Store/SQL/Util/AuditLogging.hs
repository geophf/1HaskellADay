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
import Data.Time hiding (parseTime)
import Data.Time.Clock

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.ToField hiding (Action)
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.AuditLogger
import Data.LookupTable

import Data.Time.Stamped

import Store.SQL.Util.Indexed
import Store.SQL.Util.Stamping

-- we create the ToRow instance of an audit log entry:

instance ToRow AuditEntry where
   toRow (AE app user tab col delt row act) =
      toField row:map toField [app, user, tab, delt]

-- and the field representation of an action (using the action_lk table)

instance ToField Action' where
   toField (Act' act lk) = toField (lk Map.! show act)

-- so an AuditEntry' is the indexed action value:

instance ToRow AuditEntry' where
   toRow (AE' ent lk) = toRow ent ++ [toField (aud2act' ent lk)]

-- given, of course, you have a mapping from Action -> Action'

aud2act' :: Stamped AuditEntry -> LookupTable -> Action'
aud2act' = Act' . action . stamped

{--
Great, we have the preliminaries out of the way.

make an entry in the audit table, given the next-value of a Packet and the id
of the packet table of the last packet inserted, with an audit entry with
these values:

app: "ETL"
user: "SYSTEM"
table_name: "packet"
row_number: packet id column value
action: INSERT
change: show next packet value
--}

mkAuditStmt :: String -> IxValue packet -> IO (Stamped AuditEntry)
mkAuditStmt nextPac packet =
   stampIt (AE "ETL" "SYSTEM" "packet" Nothing nextPac (idx packet) INSERT)

-- then insert that statement into the database

insertAuditEntryStmt :: Query
insertAuditEntryStmt =
   [sql|INSERT INTO audit (time,row_number,application,user_name,table_name,change,action)
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
storeAuditInfo conn activ actn nextPack pack =
   deactivatePriorAuditEntries conn activ >>
   insertAuditEntries conn actn nextPack pack

-- Fetching the active audit entry ----------------------------------------

fetchActiveAuditEntryStmt :: Query
fetchActiveAuditEntryStmt =
   [sql|SELECT    application,user_name,table_name,column_name,change,
                  row_number,action,time
        FROM      audit a
        LEFT JOIN active_lk l ON l.id = a.active_ind
        WHERE     l.active = 'ACTIVE' AND a.application = 'ETL'|]

instance FromRow AuditEntry where
   fromRow = AE <$> field <*> field <*> field <*> field
                <*> field <*> field <*> field

-- of course, to read in an audit entry, we need to read in an action ...
-- ... but I just don't care at this point:

instance FromField Action where
   fromField = \a -> const (return INSERT)

fetchActiveAuditEntry :: Connection -> IO [Stamped AuditEntry]
fetchActiveAuditEntry conn = query_ conn fetchActiveAuditEntryStmt

-- How many entries were fetched? What do they look like?
-- What was the most-recent change value?

{--
>>> withConnection (\conn -> fetchActiveAuditEntry conn >>= print)
[AE {app = "ETL", user = "SYSTEM", table = "article", column = Nothing, change = "100", row = 98, action = INSERT}]
--}

-- Convert the most recent change value to an Integer

offset :: AuditEntry -> Integer
offset = read . change

{--
>>> withConnection (\conn -> fetchActiveAuditEntry conn >>= print . map offset)
[100]
--}

oneWeekAgo :: Connection -> IO Day
oneWeekAgo =
   fmap (addDays (-7) . maximum . map (localDay . time)) . fetchActiveAuditEntry
