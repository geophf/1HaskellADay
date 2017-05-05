module Y2017.M05.D05.Solution where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

{--
Happy Cinco de Mayo, Haskellers!

Check the lock-screen of my iPad (screen shot at this directory) is says

TUESDAY, May 5th.

So, what is the most recent year where Cinco de Mayo was on a Tuesday?

... and how would you go about finding this solution?
--}

mostRecentYear5MayTuesday :: Integer
mostRecentYear5MayTuesday = mry5mt 2017

mry5mt :: Integer -> Integer
mry5mt yr = let (_,_,tues) = toWeekDate (fromGregorian yr 5 5) in
   if tues == 2 then yr else mry5mt (pred yr)

{--
>>> mostRecentYear5MayTuesday 
2015

... ooh! dat one old iPad! Yes, it is!
--}
             
