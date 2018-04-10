module Data.AuditLogger where

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

-- below import available via 1HaskellADay git repository

import Data.LookupTable
import Data.Time.Stamped

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
        action           :: Action }
      deriving (Eq, Show)

-- the actions are:

data Action = INSERT | UPDATE | DELETE
   deriving (Eq, Ord, Show)

-- but we don't know what the Action's index is. That's why we have the
-- lookup table, so we actually save an Action' as we did for the log entry
-- type. Hm. Sounds like we need a generalization here.

data Action' = Act' { act :: Action, lk :: LookupTable }

-- now, an audit record is active when insert, and deactivates all other
-- audit records when it is insert. That is to say: only the most recently
-- inserted audit record is active.
   
data Active = ACTIVE | INACTIVE
   deriving (Eq, Ord, Show) 
   
-- so with that instance declaration, we make our audit entry to-row-able:

data AuditEntry' = AE' { entry :: Stamped AuditEntry, aeLk :: LookupTable }

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
