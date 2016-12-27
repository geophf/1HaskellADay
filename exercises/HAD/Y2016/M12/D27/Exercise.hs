module Y2016.M12.D27.Exercise where

-- below imports available via 1HaskellADay git repository

import Data.SAIPE.USCounties
import Data.SAIPE.USStates
import Graph.KMeans
import Graph.ScoreCard

import Y2016.M12.D15.Exercise
import Y2016.M12.D21.Exercise

{--
It's the problem of Statehood. See the USCounty-values are 'already clustered,'
because they fall, geographically within a US State's borders, but clustering
them, as we have (see Y2016.M12.D23.Exercise), we loose the US State information.

Let's say I wish to know which US States have the most counties within a 
cluster that I have determined to be at a risk of having a high poverty rate?
What am I to do? Search the clusters by US State? How do I do that?

Let's do that.

For today's Haskell problem load up the SAIPE/poverty data at

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

cluster it, as before. Now, relate the scorecards in the clusters to US State
values. It may be helpful to know which StateAbbrev values relate to which
USState values, so, do that:
--}

type StateAbbrev = String

stateFromAbbrev :: StateAbbrev -> USState
stateFromAbbrev = undefined

-- hint: the US State-line in the SAIPE-row data preceeds US County statistics

-- From that, determine the USState value for any input USCounty value

stateFor :: USCounty -> USState
stateFor = undefined

-- hint: the StateAbbrev is embedded into the USCounty value

-- And from that, determine the USCounty values in a USState

countiesIn :: USState -> [USCounty]
countiesIn = undefined

{-- BONUS -----------------------------------------------------------------

Well, you have the clustered US County statistics. Let's do some data analysis.

1. California owns its own cluster, as LA-county is a cluster. Are there 
counties of California in every cluster?
--}

type ClusterID = Int

countiesClusteredIn :: SCClusters USCounty Axes Float -> USState -> [(USCounty, ClusterID)]
countiesClusteredIn clusters state = undefined

-- using the above, how many, or is there any, USState in only one cluster?

statesClusters :: SCClusters USCounty Axes Float -> [(USState, ClusterID)]
statesClusters clusters = undefined
