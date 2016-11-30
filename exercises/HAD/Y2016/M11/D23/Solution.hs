module Y2016.M11.D23.Solution where

import Control.Arrow (first)
import Data.Time

-- below import available from 1HaskellADay git repository

import Control.List (weave)
import Control.Logic.Frege ((<<-))

import Y2016.M11.D22.Exercise  -- for the Score-type and -values

{--
So. Yesterday, player1 was at how much again?

*Y2016.M11.D22.Solution> player1 ~> 11289979

And today, player1, a very good friend of mine, is at 11422295.

Good? Bad? Ugly?

Today's Haskell exercise.

Project the progress of player1. When does player1 exceed the score of level13?

*Y2016.M11.D22.Solution> level13 ~> 12000000

given that yesterday is 2016-11-22 and today is 2016-11-23
--}

project :: (Day, Score) -> (Day, Score) -> Score -> Day
project yesterday today = p' today (diffs yesterday today) -- one definition
-- project yesterday = fst . last <<- projection yesterday -- another definition

-- Both definitions work.

-- can we go totally points-free with the definition of project?

diffs :: (Day, Score) -> (Day, Score) -> (Integer, Score)
diffs (d0, s0) (d1, s1) = (diffDays d1 d0, s1 - s0)

p' :: (Day, Score) -> (Integer, Score) -> Score -> Day
p' d@(day, score) i goal =
   if score >= goal then day
   -- else p' (addDays dinc day, score + sinc) (dinc, sinc) goal
   else p' (incr i d) i goal

incr :: (Integer, Score) -> (Day, Score) -> (Day, Score)
incr (d, s) (day, score) = (addDays d day, s + score)

{--
*Y2016.M11.D23.Solution> let yesterday = read "2016-11-22"
*Y2016.M11.D23.Solution> let today = read "2016-11-23"
*Y2016.M11.D23.Solution> project (yesterday, player1) (today, 11422295) level13
2016-11-28
--}

-- Great!

{-- BONUS -----------------------------------------------------------------

output the projection as a time-series, e.g., a daily score update
--}

-- so we simply unwind p' to get projection ... sounds like a scan.

{--
instance (Num a, Num b) => Num (a,b) where
   (a,b) + (x,y) = (a + x, y + b)

instance (Enum a, Enum b) => Enum (a,b) where
   succ (a,b) = (succ a, succ b)
--}

projection :: (Day, Score) -> (Day, Score) -> Score -> [(Day, Score)]
projection yesterday today =
   -- let (d,s) = diffs yesterday today in
   -- takeWhile ((< goal) . snd) -- definition has to include surpassing goal
      -- (today:map (incr d s . daysToDays (fst today))
      -- [(d,s), (d + d, s + s) ..])
      -- (progression (d,s)))
      -- (zip [0, d ..] [snd today, snd today + s ..]))
   progression today (diffs yesterday today)

progression :: (Day, Score) -> (Integer, Score) -> Score -> [(Day, Score)]
progression today@(day, score) deltas goal =
     today:if score > goal then []
           else progression (incr deltas today) deltas goal

-- so, a reinvention of unfold, as it turns out

{--
     | score > goal = [(day, score)]
     | otherwise    = progression (incr d s today) (d,s) goal

progression :: (Num a, Num b) => (a,b) -> [(a,b)]
progression (x, y) = prog' (x,y) (x,y)

prog' :: (Num a, Num b) => (a, b) -> (a,b) -> [(a,b)]
prog' (x,y) (next, neyt) = (next, neyt):prog' (x,y) (next + x, neyt + y)
--}

daysToDays :: Day -> (Integer, a) -> (Day, a)
daysToDays = first . flip addDays

{--
*Y2016.M11.D23.Solution> projection (yesterday, player1) (today, 11422295) level13 ~> proj
[(2016-11-23,11422295),(2016-11-24,11554611),(2016-11-25,11686927),
 (2016-11-26,11819243),(2016-11-27,11951559),(2016-11-28,12083875)]
--}

-- question: is it 'cooler' to work with the Writer-monad here?

{-- BONUS-BONUS ----------------------------------------------------------

Chart the above projection using your favorite charting software

--}

chartSeries :: (Show x, Show y) => FilePath -> [(x, y)] -> y -> IO ()
chartSeries chartFile series limit = writeFile chartFile .
   unlines $ "Date,Score,Goal":map (showRowWith limit) series

showRowWith :: (Show x, Show y) => y -> (x, y) -> String
showRowWith z (x, y) = weave [show x, show y, show z]

-- *Y2016.M11.D23.Solution> chartSeries "Y2016/M11/D23/progression.csv" proj level13

{-- THOUGHT --------------------------------------------------------------

projection :: (a,b) -> (a,b) -> b -> m (a,b)

looks amazingly monadic, doesn't it? But change the m to a t and then it looks
very traversable/foldable. What is it?

A case can be made that projection is COmonadic, given that you condense the 
first two arguments to the function into the comonad, the result, then, is an
extension of the comonad. If you read the early papers of comonads and Haskell,
you see some mention of the experiment-function that does this projection.

So ... time-series ... comonadic?

Hm.

--}
