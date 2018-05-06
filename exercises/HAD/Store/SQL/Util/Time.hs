module Store.SQL.Util.Time where

-- converts SQL dates to Haskell days

import Data.Time
import Database.PostgreSQL.Simple.Time

d2d :: LocalTimestamp -> Maybe Day
d2d (Finite d) = Just (localDay d)
d2d _          = Nothing
