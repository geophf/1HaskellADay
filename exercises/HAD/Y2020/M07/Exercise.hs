module Y2020.M07.D17.Exercise where

{--
#MathProblem

At 6 AM, this clock was accurate. It is now 11 AM. How many minutes pass in
the real world for one minute on this clock?

(clock image at slow-clock.png)
(clock face reads 7:54 (am is implied))
--}

import Control.Arrow ((&&&))

import Data.Time

type Delta = (Time, Time)

clockVsReal :: [Delta]
clockVsReal = map (read &&& read) [("6:00", "6:00"), ("7:54", "11:00")]
