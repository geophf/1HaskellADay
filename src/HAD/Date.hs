module HAD.Date
  ( current
  , xDaysBefore
  , date
  ) where

import Data.Time
import Data.Time.Calendar.OrdinalDate

-- Helper that get the last exercise day
current :: IO (Int, Int, Int)
current = do
  day <- today
  let (_, num) = mondayStartWeek day
  return . ownGregorian . noWeekEnd $ day

xDaysBefore :: Integer -> IO (Int, Int, Int)
xDaysBefore x = do
  day <- today
  return . ownGregorian . noWeekEnd . addDays (negate x) $ day

-- Go back on Friday during the week-end
noWeekEnd :: Day -> Day
noWeekEnd day = 
  addDays (fixDay . snd . mondayStartWeek $ day) day
  where
    fixDay 6 = -1
    fixDay 7 = -2
    fixDay x = 0

    
ownGregorian :: Day -> (Int, Int, Int)
ownGregorian day = let
  (y,m,d) = toGregorian day
  in (fromInteger y, m, d)

date :: Int -> Int -> Int -> IO (Int, Int, Int)
date = ((return .) .) . (,,)

today :: IO Day
today = fmap utctDay getCurrentTime
