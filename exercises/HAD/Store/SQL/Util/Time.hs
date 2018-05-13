module Store.SQL.Util.Time where

-- converts SQL dates to Haskell days

import Data.Time
import Data.Time.Clock

import Database.PostgreSQL.Simple.Time

d2d :: LocalTimestamp -> Maybe Day
d2d (Finite d) = Just (localDay d)
d2d _          = Nothing

utc2d :: UTCTimestamp -> Maybe Day
utc2d (Finite d) = Just (utctDay d)
utc2d _          = Nothing

d2utc :: Day -> UTCTimestamp
d2utc = Finite . flip UTCTime (secondsToDiffTime 0)
