{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D17.Solution where

{--
Okay, we can read in packets (yesterday's exercise), but where to start? That
is to say: what offset do we give to read packets?

Well, if we're starting fresh, we start at 0 offset.

But, if we've done this before, we have an audit trail, so, in that case, we
need to read in the most recent, that is to say, the currently active record
(the ONLY active record, and here dependent types would be lovely, but okay)
of the audit trail.

Today's Haskell problem.

Read in the audit trail, but only read in the active record
--}

import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)

import Y2018.M01.D15.Solution

fetchActiveAuditEntryStmt :: Query
fetchActiveAuditEntryStmt =
   [sql|SELECT    application,user_name,table_name,column_name,change,
                  row_number,action,time
        FROM      audit a
        LEFT JOIN active_lk l ON l.id = a.active_ind
        WHERE     l.active = 'ACTIVE' AND a.application = 'ETL'|]

instance FromRow AuditEntry where
   fromRow = AE <$> field <*> field <*> field <*> field
                <*> field <*> field <*> field <*> (parseTime <$> field)

parseTime :: LocalTimestamp -> LocalTime
parseTime (Finite ts) = ts

-- of course, to read in an audit entry, we need to read in an action ...
-- ... but I just don't care at this point:

instance FromField Action where
   fromField = \a -> const (return INSERT)

fetchActiveAuditEntry :: Connection -> IO [AuditEntry]
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
