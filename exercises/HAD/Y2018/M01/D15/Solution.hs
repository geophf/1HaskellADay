{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D15.Solution where

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

import Data.Aeson
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField hiding (Action)
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.Logger
import Data.LookupTable

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable

import Y2017.M12.D20.Solution (Packet,next,rows)
import Y2017.M12.D27.Solution (DatedArticle)
import Y2017.M12.D29.Solution (BlockParser)
import Y2018.M01.D04.Solution (Authors,pa,storeAncilliary)
import Y2018.M01.D10.Solution (gruntWerk,readStorePacket)

{-- 
First up, load the audit actions from action_lk to get a LookupTable

hint: see above import for a function to do that

Then, given a an int 'next'-value from a Packet, make an entry in the audit log

The log entry is formatted thus:
--}

data AuditEntry =
   AE { app, user, table :: String,
        column           :: Maybe String,
        change           :: String,
        row              :: Integer,
        action           :: Action,
        time             :: LocalTime }
      deriving (Eq, Show)

instance ToRow AuditEntry where
   toRow (AE app user tab col delt row act time) =
      toField row:toField time:map toField [app, user, tab, delt]

-- the actions are:

data Action = INSERT | UPDATE | DELETE
   deriving (Eq, Ord, Show)

-- but we don't know what the Action's index is. That's why we have the
-- lookup table, so we actually save an Action' as we did for the log entry
-- type. Hm. Sounds like we need a generalization here.

data Action' = Act' { act :: Action, lk :: LookupTable }

instance ToField Action' where
   toField (Act' act lk) = toField (lk Map.! show act)

-- now, an audit record is active when insert, and deactivates all other
-- audit records when it is insert. That is to say: only the most recently
-- inserted audit record is active.
   
data Active = ACTIVE | INACTIVE
   deriving (Eq, Ord, Show) 
   
-- so with that instance declaration, we make our audit entry to-row-able:

data AuditEntry' = AE' { entry :: AuditEntry, aeLk :: LookupTable }

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

mkAuditStmt :: Packet -> IxValue article -> IO AuditEntry
mkAuditStmt pac article =
   getCurrentTime >>= \utc ->
   getCurrentTimeZone >>= \tz ->
   return (AE "ETL" "SYSTEM" "article" Nothing (show (next pac))
              (idx article) INSERT (utcToLocalTime tz utc))

-- then insert that statement into the database

insertAuditEntryStmt :: Query
insertAuditEntryStmt =
   [sql|INSERT INTO audit (row_number,time,application,user_name,table_name,change,action)
        VALUES (?,?,?,?,?,?,?)|]

insertAuditEntries :: Connection -> LookupTable -> Packet -> IxValue a -> IO ()
insertAuditEntries conn lk pac ix =
   mkAuditStmt pac ix >>=
   void . executeMany conn insertAuditEntryStmt . map (`AE'` lk) . pure

-- BUT ONLY AFTER deactivating all previously inserted audit records

deactivateAuditEntriesStmt :: Query
deactivateAuditEntriesStmt = [sql|UPDATE audit SET active_ind=?|]

deactivatePriorAuditEntries :: Connection -> LookupTable -> IO ()
deactivatePriorAuditEntries conn lk =
   void (execute conn deactivateAuditEntriesStmt (Only (lk Map.! show INACTIVE)))

-- and execute that before doing the insert, thus:

storeAuditInfo :: Connection -> LookupTable -> LookupTable -> Packet -> IxValue a -> IO ()
storeAuditInfo conn activ actn pack art =
   deactivatePriorAuditEntries conn activ >>
   insertAuditEntries conn actn pack art

{-- BONUS -----------------------------------------------------------------

Rewrite the ETL to insert an audit entry for the packet uploaded.
--}

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

-- moving Auditing to Data and Store/SQL/Util
