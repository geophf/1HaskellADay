module Y2017.M01.D04.Solution where

import Data.Array ((!))
import Data.List (sortBy)
import Data.Function (on)

-- below imports availabe via 1HaskellADay git repository

import Data.SAIPE.USStates
import Graph.ScoreCard

import Y2016.M12.D15.Solution
import Y2016.M12.D21.Solution

{--
Today we're going to go back an relook the SAIPE/poverty data posted at:

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

Today's problem? Read in the SAIPE data and score them, not by USCounty, but
by USState.
--}

readSAIPEUSStateData :: FilePath -> IO [ScoreCard USState Axes Float]
readSAIPEUSStateData =
   fmap (map scoreIt . concatMap line2USStateSummary . tail) . readSAIPERaw

scoreIt :: (USState, SAIPERow) -> ScoreCard USState Axes Float
scoreIt = SC . fst <*> arrayify . snd

line2USStateSummary :: [String] -> [(USState, SAIPERow)]
line2USStateSummary line =
   either (\state -> [(read state, line2row line)]) (const [])
          (stateContext (line !! 3))

{--
*Y2017.M01.D04.Solution> readSAIPEUSStateData "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" ~> scores
*Y2017.M01.D04.Solution> mapM_ print (take 2 scores)
SC {idx = Alabama, values = [(Population,4736374.0),(Poverty,875853.0)]}
SC {idx = Alaska, values = [(Population,720764.0),(Poverty,74941.0)]}

Now that we have the data, let's do some analysis

The State with the most people; the State with the least:

*Y2017.M01.D04.Solution> let pops = sortBy (compare `on` (! Population) . values) scores
*Y2017.M01.D04.Solution Data.Array> head pops ~> Least people:
SC {idx = Wyoming, values = [(Population,572327.0),(Poverty,60787.0)]}

*Y2017.M01.D04.Solution Data.Array> last pops ~> Most people:
SC {idx = California, values = [(Population,3.8398076e7),(Poverty,5896255.0)]}

And, of course, it's California with 30 million people, 50x its neighoring
State of Wyoming. Fancy that.

How about for poverty? Same drill:

*Y2017.M01.D04.Solution> let povs = sortBy (compare `on` (! Poverty) . values) scores
*Y2017.M01.D04.Solution Data.Array> head povs ~> least poverty:
SC {idx = Wyoming, values = [(Population,572327.0),(Poverty,60787.0)]}

*Y2017.M01.D04.Solution Data.Array> last povs ~> most poverty:
SC {idx = California, values = [(Population,3.8398076e7),(Poverty,5896255.0)]}

Same States, but that's because of population. What are the ratios of poverty
to population? We'll look at that tomorrow
--}

-- Not today, but when we have the USState SAIPE data, we'll also look at
-- USState debt data (per capita/per State) and see if there are correllations.
