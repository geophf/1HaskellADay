module Y2017.M05.D16.Solution where

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Clock
import Data.Time.LocalTime

{--
So, yesterday we were able to publish log entries with time-stamps. YAY! AND
it worked perfectly, because, well, it's 2017 and the Julian date is 123.

And the hour, minute and second were all greater than 10.

WHAT A COINCIDENCE!

That's great. But what if we didn't have these perfect storm of conincidences?
What? Are we going to throw up our hands in resignation? Are we going to throw
up our hands in disgust and resignation?

I SAY THEE NAY!

So. GIVEN an integral, and GIVEN the number of spaces it's supposed to occupy
RETURN a string of that number with zeros as prefixes.
--}

justify :: Show a => Integral a => a -> Int -> String
justify num slots =
  let n = show num
      len = slots - length n
  in  replicate len '0' ++ n

{--
SO!

>>> justify 7 3
007

Or "Bond. James Bond."
--}

-- Now, go back to yesterday's problem and roll justify into the solution.

info :: String -> IO String
info message = getCurrentTime     >>= \dayotimo ->
   let (yr, jd) = toJulianYearAndDay (utctDay dayotimo)
       TimeOfDay hr min sec = timeToTimeOfDay (utctDayTime dayotimo)
       theTime = concatMap (`justify` 2) [hr, min, floor sec]
   in  pure (justify (yr `mod` 100) 2 ++ justify jd 3 ++ theTime
         ++ (' ':message))

{--
>>> info "ALL YOUR BASE ARE BELONG TO US"
"17123232155 ALL YOUR BASE ARE BELONG TO US"
>>> info "ALL YOUR BASE ARE BELONG TO US"
"17123232200 ALL YOUR BASE ARE BELONG TO US"
>>> info "ALL YOUR BASE ARE BELONG TO US"
"17123232202 ALL YOUR BASE ARE BELONG TO US"

TA-DAH!

Oh, and all your base are belong to US! ... ICYMI

MWA-HAHAHA!
--}
