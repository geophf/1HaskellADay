module Y2016.M11.D28.Exercise where

import Control.Arrow (first)
import Data.Time

{--
We're going to be looking at the messiness of the world (or: production data)
and how to model it for use here in code.

You have the below time series:
--}

type Score = Int

timeSeries :: [(Day, Score)]
timeSeries =
   zipWith (curry (first (read . ("2016-11-2" ++) . show))) [2..]
      [11289979, 11422295, 11428000, 11564153, 11652241, 11841079]

-- so, eventually we'll look at curve-fitting (not today), but, first, let's
-- examine the messiness here. What are the gains, day-over-day?

gains :: Num a => [a] -> [a]
gains = undefined

-- The function gains takes a list of values [a,b,c,...] (assumed progressive,
-- or monotonically increasing here) and returns a list of the diffs between
-- those values: [b-a, c-b, ...]

-- (returned list is length - 1 of the input list)

-- How do you reformulate the timeSeries to make it a sensible input to gains?
-- What are the gains, day-over-day, of the timeSeries?
