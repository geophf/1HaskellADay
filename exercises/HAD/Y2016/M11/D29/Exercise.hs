module Y2016.M11.D29.Exercise where

import Data.Time

-- below import is available at 1HaskellADay git repository

import Y2016.M11.D28.Exercise

{--
One of the things about a time-series is that, well, it changes over time.
The time-series we are working with is monotomically increasing, but many
data-over-time series do not have that restriction.

Thought for later.

But, so this is increasing at some rate, yes? What rate? How do we model or
estimate the rate of change of data over time?

One way is to take the mean of all the gains. Voila! An estimate of your
progression.

A good one?

Let's see.

Today's Haskell exercise. Read in the updated time series from series.csv at
this directory, or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M11/D29/scores.csv

compute the daily gains, as you did yesterday, then find the mean gain
--}

readDailyScores :: FilePath -> IO [(Day, Score)]
readDailyScores dataFile = undefined

µgain :: (Num a, Fractional a) => [a] -> a
µgain scores = undefined

{-- BONUS -----------------------------------------------------------------

What does the mean gain give you? One way to look at the mean gain is the
slope of the linear fit of a data set. Okay, so you have the rise, you have the
run (increments of one day in the data), where is the origin?

Hm. That's another problem ... for another day. For now, let's use the first
datum as the origin.

So, with that, and using your favorite charting software, plot the (original)
data points along with the fitted curve using the µgain.

Good fit of data? Show your results.
--}

chartProgression :: FilePath -> [(Day, Score)] -> IO ()
chartProgression outputfile dailyscores = undefined
