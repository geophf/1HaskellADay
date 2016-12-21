module Y2016.M12.D21.Exercise where

import Data.Array

-- below imports available via @1HaskellADay git repository

import Data.SAIPE.USStates
import Data.SAIPE.USCounties

import Graph.ScoreCard
import Graph.KMeans

{--
So, now that we have the US county data, and as ScoreCard values.

Cluster these data.
--}

data Axes = Population | Poverty
   deriving (Eq, Ord, Show, Enum, Bounded, Ix)

clusterSAIPE :: [ScoreCard USCounty Axes Float] -> SCClusters USCounty Axes Float
clusterSAIPE counties = undefined

{--
And do this study:

1) what is the 'optimal'/most-makes-sense number of clusters?
2) what is the most populous cluster, county-population-wise and number of
   ScoreCard values-wise? What is the least populous cluster, same criteria?

Further study:
3) what associations do you see? Are there obvious City vs. Country differences?
   Other differences that are obvious?
4) Where do major cities fall? Together in one cluster? Or do they appear along
   more geographically divided lines?
--}
