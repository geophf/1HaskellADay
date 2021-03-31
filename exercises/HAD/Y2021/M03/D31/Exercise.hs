module Y2021.M03.D31.Exercise where

{--
Today, the last day of March, we're going to generate the 'Last 50 Dates,' 
make our zillions and retire to our country estate in Province, eating pealed
grapes.
--}

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- 1. get today's date (UTC is fine, or not: whatever you'd like)

today :: IO Day
today = undefined

-- 2. get the last fifty days from day, day

last50Days :: Day -> [Day]
last50Days day = undefined

{-- BONUS -------------------------------------------------------

Convert a day into POSIX seconds, because that's what some APIs require
--}

convertToPosix :: Day -> IO Integer
convertToPosix day = undefined

-- convert the last 50 dates to POSIX seconds, because that makes the date
-- perfect for her, ... or for him, ... or for both.

convertDatesToPosix :: [Day] -> IO [Integer]
convertDatesToPosix days = undefined
