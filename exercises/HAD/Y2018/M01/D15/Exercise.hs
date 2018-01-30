{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D15.Exercise where

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

import Data.Aeson
import Data.Functor.Identity (Identity)
import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField hiding (Action)
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.LookupTable (LookupTable)

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

import Y2017.M12.D20.Exercise (Packet)
import Y2017.M12.D27.Exercise (DatedArticle)
import Y2017.M12.D29.Exercise (BlockParser)

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
        time             :: Day }
      deriving (Eq, Show)

-- the actions are:

data Action = INSERT | UPDATE | DELETE
   deriving (Eq, Ord, Show)

-- but we don't know what the Action's index is. That's why we have the
-- lookup table, so we actually save an Action' as we did for the log entry
-- type. Hm. Sounds like we need a generalization here.

data Action' = Act' { act :: Action, lk :: LookupTable }

instance ToField Action' where
   toField act = undefined

-- now, an audit record is active when insert, and deactivates all other
-- audit records when it is insert. That is to say: only the most recently
-- inserted audit record is active.

data Active = ACTIVE | INACTIVE
   deriving (Eq, Ord, Show)

-- so with that instance declaration, we make our audit entry to-row-able:

data AuditEntry' = AE' { entry :: AuditEntry, aeLk :: LookupTable }

instance ToRow AuditEntry' where
   toRow ae' = undefined

-- given, of course, you have a mapping from AuditEntry -> Action'

aud2act' :: AuditEntry -> LookupTable -> Action'
aud2act' aud lk = undefined

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

mkAuditStmt :: Packet -> IxValue article -> AuditEntry
mkAuditStmt pac article = undefined

-- then insert that statement into the database

insertAuditEntryStmt :: Query
insertAuditEntryStmt =
   [sql|INSERT INTO audit (row_number,application,user,table_name,change,action)
        VALUES (?,?,?,?,?,?)|]

insertAuditEntries :: Connection -> LookupTable -> [AuditEntry] -> IO ()
insertAuditEntries conn lk entries = undefined

-- BUT ONLY AFTER deactivating all previously inserted audit records

deactivateAuditEntriesStmt :: Query
deactivateAuditEntriesStmt = [sql|UPDATE audit SET active_ind=?|]

deactivatePriorAuditEntries :: Connection -> LookupTable -> IO ()
deactivatePriorAuditEntries conn lk = undefined

-- and execute that before doing the insert, thus:

storeAuditInfo :: Connection -> Packet -> IxValue a -> IO ()
storeAuditInfo conn pack art = undefined

{-- BONUS -----------------------------------------------------------------

Rewrite the ETL to insert an audit entry for the packet uploaded.
--}

etl :: ToField a => FromJSON a => BlockParser Identity a
    -> (Connection -> [IxValue (DatedArticle a)] -> IO ())
    -> Connection -> FilePath -> IO ()
etl generator ancillaryFn conn json = undefined
