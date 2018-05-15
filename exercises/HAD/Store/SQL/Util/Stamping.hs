module Store.SQL.Util.Stamping where

import Data.Time (LocalTime)

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below import available via 1HaskellADay git repository

import Data.Time.Stamped (Stamped(Stamped), time, stamped)

instance ToRow a => ToRow (Stamped a) where
   toRow se = toField (time se):toRow (stamped se)

instance FromRow a => FromRow (Stamped a) where
   fromRow = Stamped <$> fromRow <*> (parseTimestamp <$> field) 

parseTimestamp :: LocalTimestamp -> LocalTime
parseTimestamp (Finite ts) = ts
