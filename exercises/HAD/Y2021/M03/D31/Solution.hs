module Y2021.M03.D31.Solution where

{--
Today, the last day of March, we're going to generate the 'Last 50 Dates,' 
make our zillions and retire to our country estate in Province, eating pealed
grapes.
--}

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- 1. get today's date (UTC is fine, or not: whatever you'd like)

today :: IO Day
today = utctDay <$> getCurrentTime

{--
>>> tday <- today
>>> tday
2021-03-31
--}

-- 2. get the last fifty days from day, day

last50Days :: Day -> [Day]
last50Days day = map (flip addDays day . (* (-1))) [1 .. 50] 

{--
>>> last50Days tday 
[2021-03-30,2021-03-29,2021-03-28,...,2021-02-09]
--}

{-- BONUS -------------------------------------------------------

Convert a day into POSIX seconds, because that's what some APIs require
--}

convertToPosix :: Day -> IO Integer
convertToPosix day = getCurrentTime >>= \t ->
   return (floor (utcTimeToPOSIXSeconds (t { utctDay = day })))

{--
>>> convertToPosix tday
1617198458
--}

-- convert the last 50 dates to POSIX seconds, because that makes the date
-- perfect for her, ... or for him, ... or for both.

convertDatesToPosix :: [Day] -> IO [Integer]
convertDatesToPosix = mapM convertToPosix

{--
>>> convertDatesToPosix (last50Days tday)
[1617112092,1617025692,1616939292,...,1612878492]
--}
