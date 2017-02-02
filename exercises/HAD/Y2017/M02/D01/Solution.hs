module Y2017.M02.D01.Solution where

-- below import available from 1HaskellADay git repository

import Y2017.M01.D31.Solution

{--
Okey-dokey, then! Let's GO THE DISTANCE!

or: what are the distances between the EMbary and Mars at any given time, t?

At which time t is the least-distance between the two points?

What is that distance?

TUNE IN TODAY TO FIND THE ANSWERS TO THESE QUESTIONS ... AND MORE!
--}

distance :: Triple -> Triple -> Double
distance t1 t2 = sqrt . sum $ map (uncurry . diff) [x,y,z] <*> [(t1, t2)]

diff :: (Triple -> Double) -> Triple -> Triple -> Double
diff f t1 t2 = let d = f t1 - f t2 in d * d

{--
*Y2017.M02.D01.Solution> readEpochs "Y2017/M01/D31/de102/ascm0800-2recs.102" 
*Y2017.M02.D01.Solution> let earthEpochs = it
*Y2017.M02.D01.Solution> let ptEarth = head . points $ head earthEpochs 
*Y2017.M02.D01.Solution> readEpochs "Y2017/M01/D31/de102/ascm1100-2recs.102" 
*Y2017.M02.D01.Solution> let marsEpochs = it
*Y2017.M02.D01.Solution> let ptMars = head . points $ head marsEpochs 
*Y2017.M02.D01.Solution> (ptEarth , ptMars)
(ThreeD {x = 1428816.5, y = 1428880.5, z = 2.548607354903266e7},
 ThreeD {x = 1319248.5, y = 1319312.5, z = -3.467507395634581e7})
*Y2017.M02.D01.Solution> distance ptEarth ptMars ~> 6.016134705487531e7
--}

-- For the next set of questions assume time (t) proceeds thus: t=0,1,2,...

-- what is the distance between EMbary and Mars at t = 0?

distanceAtTime :: Epoch -> Epoch -> Int -> Double
distanceAtTime embary mars t =
   let idx = (!! t) . points in
   distance (idx embary) (idx mars)

-- recall the ephemeris data is located at Y2017/M01/D30/de102/
-- and you can read in those data by useing readEpochs from the imported module.

{--
*Y2017.M02.D01.Solution> distanceAtTime (head earthEpochs) (head marsEpochs) 0
6.016134705487531e7
--}

-- Of course, indexing into the depth/tail of a list has a O(n)-cost. Not good.
-- Let's, then, find all the distances and marry those distances to time t.

-- why? Well, define the function first, please.

allDistances :: Epoch -> Epoch -> [(Double, Int)]
allDistances earth mars = 
   zip (map (uncurry distance) (zip (points earth) (points mars))) [0..]

{--
*Y2017.M02.D01.Solution> let epoch1 = allDistances (head earthEpochs) (head marsEpochs)
*Y2017.M02.D01.Solution> take 5 epoch1
[(6.016134705487531e7,0),(7.479405020129974e7,1),(725283.7205509734,2),
 (13452.545295362504,3),(169.0253054274161,4)]
--}

-- For the first epoch, at what time, t, are the earth and mars closest?
-- Same for the second (or last with these data) epoch.

{--
first epoch:
*Y2017.M02.D01.Solution Control.Arrow> minimum epoch1
(3.201902583115096e-5,107)
*Y2017.M02.D01.Solution Control.Arrow> minimum epoch2
(3.498218255328639e-5,107)
--}

-- Of course, the time to launch a satellite may be quite different than at the
-- time Earth and Mars are at their closest point. We'll start to look at
-- satellite mission planning tomorrow.

{-- BONUS -----------------------------------------------------------------

Using whatever plotting software that represent three dimensions, display the
Earth and Mars in orbit about the Sun.
--}

plotit :: FilePath -> [[Epoch]] -> IO ()
plotit outputPlot planets = undefined
