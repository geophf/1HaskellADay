module Y2016.M11.D29.Solution where

import Control.Arrow ((&&&), second)
import Data.Time

-- below imports is available at 1HaskellADay git repository

import Control.List (weave)
import Control.Presentation (laxmi)
import Control.Scan.CSV (csv)
import Y2016.M11.D28.Solution

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
readDailyScores = 
   fmap (map ((read . head &&& read . last) . csv) . tail . lines) . readFile

{--
*Y2016.M11.D29.Solution> readDailyScores "Y2016/M11/D29/scores.csv" ~> scores
[(2016-11-22,11289979),(2016-11-23,11422295),...]
--}

µgain :: (Num a, Fractional a, RealFrac a) => [a] -> a
µgain = uncurry (/) . (sum . gains &&& fromIntegral . length)

-- *Y2016.M11.D29.Solution> µgain (map (toRational . snd) it) ~>
-- 779172 % 7 or ~ 111310 for us mere humans

-- (side note/gripe: I wish we could control output à la Prolog so that
-- rational values had a yummier taste to my eyes, but That's Just Me (tm))

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

chartProgression :: Show a => FilePath -> [(a, Rational)] -> IO ()
chartProgression outputfile dailyscores = writeFile outputfile .
   unlines . ("Date,Score,Fitted Score":) $ extension dailyscores

extension :: Show a => [(a, Rational)] -> [String]
extension scores@((_, score):_) =
   ext scores (toRational score) (µgain (map (toRational . snd) scores))

ext :: Show a => [(a, Rational)] -> Rational -> Rational -> [String]
ext [] _ _ = []
ext ((d,s):rest) score add = weave [show d, lax s, lax score]
   :ext rest (score + add) add
      where lax = laxmi 2

{--
*Y2016.M11.D29.Solution> readDailyScores "Y2016/M11/D29/scores.csv"       >>=
                         chartProgression "Y2016/M11/D29/progression.csv"
                       . map (second toRational)
--}
