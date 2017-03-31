module Y2017.M03.D31.Exercise where

import Data.Array
import Data.Map (Map)

-- below import available via 1HaskellADay git repository

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
mph2mps milesperhour = undefined

-- Now, from the chart, extract the data points you need to answer the below.

data Impact = Slight | Serious | Fatal deriving (Eq, Ord, Show)

type Chart = Map Impact (Array MPH Percentage)

readChart :: FilePath -> IO Chart
readChart file = undefined

-- The function chart translates the slight/severe/fatal data sets to arrays
-- The data are located at this directory as impact-data.csv

-- 1. Between which to mph speeds do most severe injuries occur? What does that
-- translate to in m/s?

severe :: Chart -> (MPH, MPH)
severe impactData = undefined

-- 2. same question for fatalities

fatalities :: Chart -> (MPH, MPH)
fatalities impactData = undefined

-- 3. When do slight injuries make a prominent appearance when cars collide 
-- with pedestrians?

slights :: Chart -> (MPH, MPH)
slights impactData = undefined

-- Be safe out there, Haskellers!
