module Y2017.M05.D15.Solution where

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Clock
import Data.Time.LocalTime

{--
AHA!

So, we need to log application events like:

"Would you like to play a game of chess?"

and

"Self-destruct sequence commencing in 5..."

and stuff like that. You know: the usual application log stuff.

But this is how we time-stamp the logged messages:

YYJJJTTTTTT

   where
     YY is the last two digits of they year, because Y2K taught us ... NOTHING!
     JJJ is the Julian day of the year; and,
     TTTTTT is the hour-minute-second of the log entry.

(Now don't ask me if that zulu time or not, because I just don't care.)

Now, in some programming language that starts with a 'J' this is very
... 'simple' to do.

CHA! RIGHT!

http://www.rgagnon.com/javadetails/java-0506.html

So, let's do this in Haskell.

GIVEN a message to log, OUTPUT that message with the above time-stamp.
--}

info :: String -> IO String
info message = getCurrentTime     >>= \dayotimo ->
   let (yr, jd) = toJulianYearAndDay (utctDay dayotimo)
       TimeOfDay hr min sec = timeToTimeOfDay (utctDayTime dayotimo)
       theTime = show hr ++ show min ++ show (floor sec)
   in  pure (drop 2 (show yr) ++ show jd ++ theTime ++ (' ':message))

{-- e.g.:

>>> info "ZO RELAXEN UND WATSCHEN DER BLINKENLICHTEN."
"17135114823 ZO RELAXEN UND WATSCHEN DER BLINKENLICHTEN."

>>> info " IST NICHT FÜR GEWERKEN BEI DUMMKOPFEN. DER RUBBERNECKEN SIGHTSEEREN KEEPEN DAS COTTONPICKEN HÄNDER IN DAS POCKETS MUSS."
"1712219495  IST NICHT F\220R GEWERKEN BEI DUMMKOPFEN. DER RUBBERNECKEN SIGHTSEEREN KEEPEN DAS COTTONPICKEN H\196NDER IN DAS POCKETS MUSS."

So there you have it. Haskell, Blinkenlichten, and a wee bit-o-German...ish
thrown in, to boot!

Of course, this solution works because we have 

year xx        > 9 and 
julian day yyy > 99

WHAT HAPPENS IN OTHER CASES?!?

We'll look at that tomorrow.
--}
