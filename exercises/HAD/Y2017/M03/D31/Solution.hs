module Y2017.M03.D31.Solution where

import Data.Array
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-), adjoin)
import Control.Scan.CSV
import Data.Percentage

{--
Take a look at Fig. 2.6 from the Road Safety Web Publication No. 16
Relationship between Speed and Risk of Fatal Injury: Pedestrians and Car 
Occupants at the URL:

http://nacto.org/docs/usdg/relationship_between_speed_risk_fatal_injury_pedestrians_and_car_occupants_richards.pdf

(fig 2.6 is also screen-capped at this directory)

Okay, fine, you're doing a study of slight and severe injuries as well as 
fatalities based on these data collected, however, you need to do your reports
in m/s, that is: meters per second, instead of in mph or miles per hour.

Today's Haskell problem. Write a mph2mps converter. Research the conversions
as needed
--}

type MPH = Int

mph2mps :: MPH -> Float
mph2mps = (/ secondsPerHour) . (* metersPerMile) . fromIntegral
  where metersPerMile = 1609.34
        secondsPerHour = 3600

{--
>>> mph2mps 20
8.940778
>>> mph2mps 30
13.411166
--}

-- Now, from the chart, extract the data points you need to answer the below.

data Impact = Slight | Serious | Fatal deriving (Eq, Ord, Enum, Ix, Show)

type Chart = Array Impact (Map MPH Percentage)

readChart :: FilePath -> IO Chart
readChart file = chartify . drop 3 . lines <$> readFile file

chartify :: [String] -> Chart
chartify = toChart . transpose . boxInts

-- so now we just define all the functions named above 

boxInts :: [String] -> [[Int]]
boxInts = map (map read . csv)

toChart :: [[Int]] -> Chart
toChart (mphs:threes) =
   let perc3s = map (map (P . (/ 100) . fromIntegral)) threes
       arrs   = map (Map.fromList . zip mphs) perc3s in
   listArray (Slight, Fatal) arrs

-- The function chart translates the slight/severe/fatal data sets to arrays
-- The data are located at this directory as impact-data.csv

-- All three questions ask the same question, just for different data sets
-- We refold the data as a set of differences and extract the max rise

maxRise :: [(MPH, Percentage)] -> (MPH, MPH)
maxRise = snd . maximum . diffs

diffs :: [(MPH, Percentage)] -> [((Rational, Rational), (MPH, MPH))]
diffs = scanr (\(mph, perc) ((_, r), (mph1,_)) ->
              let p = percent perc in ((r - p, p), (mph, mph1))) ((0, 0), (0,0))

{--
Yeah, you just saw that. I just used scanr.

>>> diffs (Map.toList (ch ! Slight))
[((1 % 5,0 % 1),(0,10)),((3 % 5,1 % 5),(10,20)),((1 % 5,4 % 5),(20,30)),
 ((0 % 1,1 % 1),(30,40)),((0 % 1,1 % 1),(40,50)),((0 % 1,1 % 1),(50,60)),
 ((0 % 1,1 % 1),(60,70)),(((-1) % 1,1 % 1),(70,0)),((0 % 1,0 % 1),(0,0))]
>>> maxRise (Map.toList (ch ! Slight))
(10,20)
--}

-- 1. Between which two mph speeds do most severe injuries occur? What does that
-- translate to in m/s?

severe :: Chart -> (MPH, MPH)
severe = maxRiseKind Serious

{--
>>> severe ch
(10,20)
>>> adjoin mph2mps (severe ch)
(4.470389,8.940778)
--}

-- 2. same question for fatalities

fatalities :: Chart -> (MPH, MPH)
fatalities = maxRiseKind Fatal

{--
>>> fatalities ch
(20,30)
>>> adjoin mph2mps (fatalities ch)
(8.940778,13.411166)
--}

-- 3. When do slight injuries make a prominent appearance when cars collide 
-- with pedestrians?

slights :: Chart -> (MPH, MPH)
slights = maxRiseKind Slight

{--
>>> slights ch
(10,20)
>>> adjoin mph2mps (slights ch)
(4.470389,8.940778)
--}

maxRiseKind :: Impact -> Chart -> (MPH, MPH)
maxRiseKind = maxRise . Map.toList <<- flip (!)

-- Be safe out there, Haskellers!
