module Y2016.M12.D23.Solution where

import Codec.Compression.GZip
import Control.Monad (void)
import System.Environment

-- below imports available from 1HaskellADay git repository

import Data.SAIPE.USCounties
import Graph.KMeans
import Graph.ScoreCard
import Graph.ScoreCard.Clusters

import Y2016.M12.D15.Solution
import Y2016.M12.D20.Solution
import Y2016.M12.D21.Solution

{--
So, a couple of days ago we were able to cluster US Counties by SAIPE/poverty
statistics. Great!

Data is gzipped here:

Y2016/M12/D15/SA IPESNC_15DEC16_11_35_13_00.csv.gz

The problem is that there are so many counties. And, clustering them, we lost
our reference to their US States.

Well, clusters are supposed to help by grouping large data sets into (smaller)
groups. Today, as a first step, let's visualize these clusters.

Using whatever data visualization tool you prefer, show the US Counties in
their clusters.=

So, first we load the data and create score cards from them

*Y2016.M12.D23.Solution>  readSAIPERaw "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" ~> raw
--}

showClusteredUSCounties :: [ScoreCard USCounty Axes Float] -> IO ()
showClusteredUSCounties scorecards = getEnv "CYPHERDB_ACCESS" >>= \endpoint ->
   let (_gens, clusters) = kmeans 30 scorecards
       colors            = colorization clusters scorecards
   in  void (relateClusteredCells endpoint (NC ("US Counties", colors)))

{--
Now we upload this information to our graph database:
*Y2016.M12.D23.Solution> showClusteredUSCounties (saipeRows2SC raw)
... ,\"errors\":[]}\n"

Now let's add the clustering nodes:
*Y2016.M12.D23.Solution> let cclusters = relateClusters (NC ("US Counties", colors))
*Y2016.M12.D23.Solution> uploadClusters endpoint cclusters 
...\"errors\":[]}\n"

Sample graphs show in this directory.
--}
