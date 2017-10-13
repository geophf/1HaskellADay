module Y2016.M10.D24.Solution where

import Control.Arrow ((&&&))
import Data.Array
import qualified Data.Map as Map

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658

-- below modules available from 1HaskellADay git repository

import Graph.KMeans
import Graph.ScoreCard

import Y2016.M10.D19.Solution (LeadersLosers)
import Y2016.M10.D21.Solution (top5s2ScoreCards)

{--
So, 'yesterday' (Friday) you were able to take a set of daily stock reports

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

and from that report give a set of score cards, one per stock, on their top5s
showings.

Great.

So now you have score cards, which are the feed to a little k-means algorithm
we developed back in March 2016.

So, today: cluster the score cards using the k-means algorithm.
--}

type NClusters = Int

type Top5sScore a = ScoreCard Symbol LeadersLosers a

kmeansMeBaybee :: (Real a, Num a) => NClusters -> [Top5sScore a]
               -> (Int, SCClusters Symbol LeadersLosers Rational)
kmeansMeBaybee clusts = kmeans clusts . map ratify

ratify :: (Num a, Real a) => Top5sScore a -> Top5sScore Rational
ratify = uncurry SC . (idx &&& ratifyArr . values)

ratifyArr :: (Ix x, Num a, Real a) => Array x a -> Array x Rational
ratifyArr = uncurry listArray . (bounds &&& map toRational . elems)

{--
*Y2016.M10.D24.Solution> top5s2ScoreCards "Y2016/M10/D17/top5s.csv" ~> tops

when running the kmeans we eventually arrive at a div by 0 error, here's why:

*Y2016.M10.D24.Solution> epoch it
fromList [(1,(Sum {getSum = 9},a DList)),(2,(Sum {getSum = 1397},a DList)),(4,(Sum {getSum = 2},a DList)),(5,(Sum {getSum = 39},a DList))]
*Y2016.M10.D24.Solution> epoch it
fromList [(1,(Sum {getSum = 8},a DList)),(2,(Sum {getSum = 1403},a DList)),(4,(Sum {getSum = 1},a DList)),(5,(Sum {getSum = 35},a DList))]
*Y2016.M10.D24.Solution> epoch it
fromList *** Exception: Ratio has zero denominator

The middle cluster (2) consumes the clusters around it until cluster 4 has 
just 1 element. Let's fix this in kmeans ... DONE!

so:
*Y2016.M10.D24.Solution> let (gens, ans) = kmeansMeBaybee 5 (Map.elems tops)

Question: how many generations did it take to arrive a this number of clusters?
*Y2016.M10.D24.Solution> gens ~> 18

Questions: what number of clusters is 'optimal,' according to you? 5? 10? more?
*Y2016.M10.D24.Solution> length ans ~> 4

5 requested clusters converges to 4 actual clusters
10 requested clusters converges to 8 actual clusters
30 requested clusters converges to (this is taking a while...) 8 actual clusters
in 138 generations.

8 seems to be what k-means likes.
--}

{-- BONUS -----------------------------------------------------------------

There is such a thing as scale. If a particular attribute, say, volume, far
out-shows all the others, it then may become dominant, overshadowing possibly
significant differences in other attributes.

So, for the bonus, scale all the attributes on the continuum (or near enough
for floats) from 0.0 - 1.0
--}

scale :: [Top5sScore Int] -> [Top5sScore Float]
scale = undefined

-- hint: scale may (or may not) be comonadic.

-- now, with the score cards rescaled: what are the new results from the
-- k-means clustering? Do the new clusters make more or less sense to you?

{-- BONUS-BONUS -----------------------------------------------------------

Graph the clustered results, giving a visual representation of your clusters.

Use whatever drawing/graphing tool you like.

--}

grapheme :: SCClusters a b c -> IO ()
grapheme clusters = undefined
