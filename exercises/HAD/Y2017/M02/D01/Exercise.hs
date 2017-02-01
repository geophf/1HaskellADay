module Y2017.M02.D01.Exercise where

-- below import available from 1HaskellADay git repository

import Y2017.M01.D31.Exercise

{--
Okey-dokey, then! Let's GO THE DISTANCE!

or: what are the distances between the EMbary and Mars at any given time, t?

At which time t is the least-distance between the two points?

What is that distance?

TUNE IN TODAY TO FIND THE ANSWERS TO THESE QUESTIONS ... AND MORE!
--}

distance :: Triple -> Triple -> Double
distance = undefined

-- Hint: we know that the distance between two vectors is what?
-- (ooh! math-y! ... math-ish? NO: MATH-IC!)

-- For the next set of questions assume time (t) proceeds thus: t=0,1,2,...

-- what is the distance between EMbary and Mars at t = 0?

distanceAtTime :: Epoch -> Epoch -> Int -> Double
distanceAtTime embary mars t = undefined

-- recall the ephemeris data is located at Y2017/M01/D30/de102/
-- and you can read in those data by useing readEpochs from the imported module.

-- Of course, indexing into the depth/tail of a list has a O(n)-cost. Not good.
-- Let's, then, find all the distances and marry those distances to time t.

-- why? Well, define the function first, please.

allDistances :: Epoch -> Epoch -> [(Double, Int)]
allDistances earth mars = undefined

-- For the first epoch, at what time, t, are the earth and mars closest?
-- Same for the second (or last with these data) epoch.

-- Of course, the time to launch a satellite may be quite different than at the
-- time Earth and Mars are at their closest point. We'll start to look at
-- satellite mission planning tomorrow.

{-- BONUS -----------------------------------------------------------------

Using whatever plotting software that represent three dimensions, display the
Earth and Mars in orbit about the Sun.
--}

plotit :: FilePath -> [[Epoch]] -> IO ()
plotit outputPlot planets = undefined
