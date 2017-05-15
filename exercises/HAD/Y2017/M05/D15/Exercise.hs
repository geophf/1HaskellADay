module Y2017.M05.D15.Exercise where

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Clock

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
info message = undefined

{-- e.g.:

>>> info "ZO RELAXEN UND WATSCHEN DER BLINKENLICHTEN."
"17135114823 ZO RELAXEN UND WATSCHEN DER BLINKENLICHTEN."

--}
