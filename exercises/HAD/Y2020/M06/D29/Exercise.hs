module Y2020.M06.D29.Exercise where

import Data.Time

{--
Okay! So, howdy!

Today's haskell exercise, we're going to ... wait for it ... EXERCISE!

Let's say we've got this person exercising and tracking their steps, AND
setting goals, because that's what some exercisers do: set goals.

1. write a function that, given the date, computes the number of days to the
current date.

(so, January 2, 2020 would give 2 days)

(hint: check your library functions)
--}

days :: Day -> Int
days d = undefined

{--
2. Given the current date, and the average steps walked so far, and, a new
piece of information, the number of steps walked today, compute the new yearly
step average (an int, please)
--}

averageSteps :: Day -> Int -> Int -> Int
averageSteps date ave stepsToday = undefined

{--
3. So, my goal is to average x steps per day for the year. How many steps must
I walk each day to do that?
--}

goalSteps :: Day -> Int -> Int -> Int
goalSteps date ave goal = undefined

-- so, goalSteps will say: "Hey, you need to walk 14000 steps per day to 
-- average 12000 steps per day for the year."
