module Y2016.M11.D23.Exercise where

import Data.Time

-- below import available from 1HaskellADay git repository

import Y2016.M11.D22.Exercise

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
project (yesterday, score0) (today, score1) level13 = undefined

-- Great!

{-- BONUS -----------------------------------------------------------------

output the projection as a time-series, e.g., a daily score update
--}

projection :: (Day, Score) -> (Day, Score) -> Score -> [(Day, Score)]
projection yesterdayScore todayScore maxScore = undefined

-- question: is it 'cooler' to work with the Writer-monad here?

{-- BONUS-BONUS ----------------------------------------------------------

Chart the above projection using your favorite charting software

--}

chartSeries :: FilePath -> [(x, y)] -> IO ()
chartSeries chartFile series = undefined

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
