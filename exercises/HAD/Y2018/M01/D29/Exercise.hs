{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D29.Exercise where

{--
Friday last we downloaded a week's-worth of articles from a REST endpoint with
the end-game of storing the new and updated articles.

Okay, fine. Good.

But did we accomplish our aim? With a daily process running along, yeah, sure,
but what happens when we miss a day or two or more?

We need to know what the state of the database is to determine how far into the
REST's archive we have to dig to make our database au courant!

How do we do that?

1. we can query the article database to get the last article stored (and, to
   the point: the date of that article).

... but as our archive grows, that incurs a full-table scan of the article
table to get just one piece of information. The most recent date has cost N
for N articles. Can we do better?

Yes, we can.

2. The archive table has 1 active date-stamped entry, but even should we scan 
   that full table to get that one entry, there is only one audit entry per 
   packet of articles. A packet may contain up to 100 articles (normally), so 
   the cost is a much lower one for that datum.

Today's Haskell problem. Find out when the last ETL upload was completed and
return that date, minus one week. Then, with that information, return the set
of packets of articles from the present to that derived date.
--}

import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time hiding (Date)

-- below imports available via 1HaskellADay git repository

-- import Y2018.M01.D15.Exercise (time) -- see bonus

import Y2018.M01.D17.Exercise (fetchActiveAuditEntry)
import Y2018.M01.D26.Exercise

oneWeekAgo :: Connection -> IO Day
oneWeekAgo conn = undefined

-- then get all the packet from now to that derived date in the manner already
-- defined in Y2018.M01.D26.Exercise

-- Given the value from oneWeekAgo, how many packets did you download from the
-- REST endpoint?

{-- BONUS -----------------------------------------------------------------

Okay, there's a problem with audit log entries. There is a time-column in the
database, but it is nowhere referenced in the Haskell-type.

Why?

Because I was stupid then.

Alles-o-twitter: And you're not now?
Me: no comment.

So, there's a couple of ways to go about this.
--}

-- 1. just ask the database:

fetchLastUpdateStmt :: Query
fetchLastUpdateStmt =
   [sql|SELECT    a.time
        FROM      audit a
        LEFT JOIN active_lk l ON l.id = a.active_ind
        WHERE     l.active = 'ACTIVE' and a.application = 'ETL'|]

fetchLastUpdated :: Connection -> IO [Day]
fetchLastUpdated conn = undefined

-- Of course, it would be nice for Day to have a FromRow-instance, wouldn't it?

data Date = Date LocalTimestamp
   deriving (Eq, Ord, Show)

day :: Date -> Day
day (Date (Finite ts)) = undefined

-- Haskell's simple PostgreSQL uses their own date/time representation

instance FromRow Date where
   fromRow = undefined

-- and get the maximum there (should be only one value, but anyway)

-- 2. Or, you could enhance the AuditEntry-type with a time field and update
--    the SQL fetch function in Y2018.M01.D17 to include that field.

-- Player's choice here.

-- What answer did you get for approach 1? approach 2? Were they the same?

-- Tomorrow we'll look at extracting article metadata from out data store
-- to determine the types we downloaded from the REST endpoint: NEW, UPDATED,
-- or REDUNDANT.
