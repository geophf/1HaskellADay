module Store.SQL.Util.Time where

-- converts SQL dates to Haskell days

import qualified Data.ByteString.Char8 as B
import Data.Time
import Data.Time.Clock

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.Types (Query(Query))

import Store.SQL.Util.TaggedTypes (TaggedType, untag)

d2d :: LocalTimestamp -> Maybe Day
d2d (Finite d) = Just (localDay d)
d2d _          = Nothing

utc2d :: UTCTimestamp -> Maybe Day
utc2d (Finite d) = Just (utctDay d)
utc2d _          = Nothing

d2utc :: Day -> UTCTimestamp
d2utc = Finite . flip UTCTime (secondsToDiffTime 0)

-- We need to get the most recent day from a data-table

type TableName = String
type ColumnName = String

latest :: Connection -> TableName -> ColumnName -> IO Day
latest conn tableName dateColumn =
   let query = Query (B.pack ("SELECT max(" ++ dateColumn ++ ") FROM "
                     ++ tableName)) in
   untag <$> tday conn query

tday :: Connection -> Query -> IO (TaggedType Day)
tday conn qury = head <$> query_ conn qury
