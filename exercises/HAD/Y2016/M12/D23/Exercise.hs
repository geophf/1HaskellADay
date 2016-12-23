module Y2016.M12.D23.Exercise where

import Codec.Compression.GZip

-- below imports available from 1HaskellADay git repository

import Data.SAIPE.USCounties
import Graph.KMeans
import Graph.ScoreCard
import Graph.ScoreCard.Clusters

import Y2016.M12.D15.Exercise
import Y2016.M12.D20.Exercise hiding (USCounty)
import Y2016.M12.D21.Exercise

{--
So, a couple of days ago we were able to cluster US Counties by SAIPE/poverty
statistics. Great!

Data is gzipped here:

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

The problem is that there are so many counties. And, clustering them, we lost
our reference to their US States.

Well, clusters are supposed to help by grouping large data sets into (smaller)
groups. Today, as a first step, let's visualize these clusters.

Using whatever data visualization tool you prefer, show the US Counties in
their clusters.
--}

showClusteredUSCounties :: SCClusters USCounty Axes Float -> IO ()
showClusteredUSCounties clusters = undefined

-- hint: ... nah. I mean, you can use the above import, if you like.
