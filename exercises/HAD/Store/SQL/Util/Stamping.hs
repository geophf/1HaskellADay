module Store.SQL.Util.Stamping where

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below import available via 1HaskellADay git repository

import Data.Stamped (Stamped, time, stamped)

instance ToRow a => ToRow (Stamped a) where
   toRow se = toField (time se):toRow (stamped se)
