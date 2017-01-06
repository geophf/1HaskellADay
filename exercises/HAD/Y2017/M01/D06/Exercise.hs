module Y2017.M01.D06.Exercise where

import Data.Array

-- below modules available via 1HaskellADay git repository

import Graph.Query
import Graph.ScoreCard

-- for reference and types:
import Graph.KMeans hiding (kmeans)

import Graph.ScoreCard.Clusters

import Y2016.M12.D26.Exercise
import Y2017.M01.D05.Exercise

{--
Yesterday we created a combined scorecard from the SAIPE/poverty data and the
US State debt total and per capita data. We also looked at poverty ratios of
States. YAY!

We have an interminging of types of data in our resulting scorecards: 
populations, which are large numbers, and ratios, which are small ones, and
we have another type thrown in: debt, which is even larger than population.

So, how do we scale these disparate number sets so they can be compared without
one drowning out the other as noise?

Today's haskell problem.

Given a ScoreCard set, [sc], scale all the attributed data so they can be
compared, 1-to-1 ... howsoever you take that meaning:
--}

scale :: (Ix a, Ix b, RealFrac c) => [ScoreCard a b c] -> [ScoreCard a b c]
scale scorecards = undefined

-- using the resulting scorecard set you got yesterday, rescale US State
-- poverty and debt data:

rescaleUSStateData :: [EnhancedSC] -> [EnhancedSC]
rescaleUSStateData stateScorecards = undefined

{--
2. Clustering.

The KMeans algorithm gets bogged down over time, the larger the data sets
become. Is there a better clustering algorithm, time-wise, when the clusters
are unknown? Please recommend.

Until I get a better one, let's see if we can help KMeans along. Part of the
computation involves calculating the centroid of each scorecard. Now, we have
done this before (see Graph.KMeans and the solution for Y2016.M12.D26.Exercise)
but, as you see in Graph.KMeans, we recompute the centriod at every pass for
every scorecard.

Well, once you've established the data for a scorecard, its centroid is fixed!

Let's write an ENHANCED scorecard (not just 'Enhanced' scorecard, but all-caps)
that incorporates the centroid as part of its sine qua non.
--}

data EnhancedScoreCard a b c = ESC { centroid :: a, scorecard :: ScoreCard a b c }
   deriving (Eq, Show)

enhanceScoreCard :: ScoreCard a b c -> EnhancedScoreCard a b c
enhanceScoreCard scorecard = undefined

-- which simply embeds the centroid of the scorecard into the new value

-- Now, with the enhanced score cards from the US State data, cluster them:

type EnhancedClusters a b c = Clusters Int (Cluster (EnhancedScoreCard a b c))

kmeans :: Int -> [EnhancedScoreCard a b c] -> (Int, EnhancedClusters a b c)
kmeans nclusters scorecards = undefined

-- which results in (n, clusters) in n epochs.

-- What are the clusters now? Are there significant differences in the number
-- and sized of clusters now that we are clustering by State and including
-- debt data along with poverty data?

-- does the time to cluster these data change significantly? Try clustering
-- the US County data (as you did in Y2016.M12.D26.Exercise) with the enhanced
-- scorecards. Do they cluster faster or slower than the non-enhanced scorecards?

{-- BONUS -----------------------------------------------------------------

Graph the clusters of the enhanced scorecards. What does the resulting graph
look like? Share your results.
--}

graphEnhancedClusters :: Endpoint -> EnhancedClusters a b c -> IO ()
graphEnhancedClusters url clusters = undefined

-- hint: use Graph.ScoreCard.Clusters functions to help here
