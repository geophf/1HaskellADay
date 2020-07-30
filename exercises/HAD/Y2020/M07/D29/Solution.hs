module Y2020.M07.D29.Solution where

{--
"How many days from today until ...?"

'Days' are an interesting concept, thanks to The Real World(tm) calendar.
So, computing number of days from here to there is not a metric calculation.

Today, we're going to compute the number of days until ... April 15th, 2021,
say, or some other day in the next year, or so.
--}

import Data.Time
import Data.Time.Calendar
import Data.Time.Clock

daysTo :: Day -> IO Integer
daysTo someDateInTheFuture =
   getCurrentTime >>= return . diffDays someDateInTheFuture . utctDay

{--
>>> daysTo (read "2021-04-15")
260
--}
