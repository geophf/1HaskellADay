{-# LANGUAGE ViewPatterns #-}

module Graph.KMeans where

-- http://lpaste.net/3576182129349885952

import Control.Arrow
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Control.DList                            -- http://lpaste.net/107607
import Control.Logic.Frege ((<<-), adjoin)      -- http://lpaste.net/111101
import Graph.ScoreCard                  -- http://lpaste.net/7322735479504240640

-- the below imports are for demonstrating k-means clustering with Top5s stocks

import Graph.Query                      -- http://lpaste.net/6813513488191717376
import Graph.ScoreCard.Loader           -- http://lpaste.net/1163865820011429888
import Graph.Top5sAppearances           -- http://lpaste.net/4928100283508064256

{-- A solution to the problem posted at http://lpaste.net/5690415111206862848
@1HaskellADay solution for 2016-03-25

ref: stock-market/clusterScoreCards.hs

Okay, yesterday we reified Top5sAppearances meta-data as ScoreCards. The thing
is ... this file won't compile now because the KMeans module won't compile 
anymore because Graph.ScoreCard has changed in its indices. Try to load this
file now with the KMeans module import uncommented.

...

--}

{--
When you do do that, you get the following set of errors:

Algorithmic/Clustering/KMeans.hs:102:32:
    Not in scope: type constructor or class ‘Anomaly’

Algorithmic/Clustering/KMeans.hs:191:20:
    Not in scope: type constructor or class ‘Anomaly’

Algorithmic/Clustering/KMeans.hs:191:33:
    Not in scope: type constructor or class ‘Anomaly’

Algorithmic/Clustering/KMeans.hs:204:45: Not in scope: ‘anomalies’
--}

{--
But after you get these errors resolved, then you have the whole 'ScoreCard
is a * -> * -> * -> * type not a * type problem.

Kmeans needs to be rewritten.

Today's haskell problem.

Write a KMeans clustering algorithm ... that works with Graph.ScoreCard.

see, e.g.: https://www.youtube.com/watch?v=zHbxbb2ye3E
--}

type Clusters key scorecard = Map key (Cluster scorecard)
type SCClusters a b c = Clusters Int (ScoreCard a b c)

{--
Okay.

The k-means, geophf-style.

Take the homogeneous mess, divide them into clusters, determine cluster
centers, then redistribute clusters so that cells gravitate to their cluster
centers. Repeat until stability reached, that is, all clusters in this epoch
have all the same cells as all clusters in the last epoch. YAY!

So we need 1) clusters, 2) cluster centers, and clusters need to be comparable.
--}

type Cluster scorecard = (Sum Int, DList scorecard)
type SCCluster a b c = Cluster (ScoreCard a b c)

-- we also need an efficient way of inserting cells into clusters (eventually)

add1 :: Ord k => k -> a -> Clusters k a -> Clusters k a
add1 k val = Map.insertWith (<>) k (Sum 1, dl' val)

type Center = Array

computeClusterCenter :: (Ix b, Fractional c) => SCCluster a b c -> Center b c

-- we ... 'assume' all vectors are of uniform length, so we can just put
-- them into a matrix and mean them

computeClusterCenter (map values . dlToList . snd -> arrs@(samp:_)) =
   listArray (bounds samp)
             ((uncurry map . first (flip (/))) (sumDown (map elems arrs)))

-- I'm feeling very LISPy rn --^

sumDown :: Fractional ä => [[ä]] -> (ä, [ä])
sumDown = uncurry (sd' 0) . (head &&& tail)

sd' :: Fractional ä => Int -> [ä] -> [[ä]] -> (ä, [ä])
sd' x list [] = (fromRational $ fromIntegral x, list)
sd' x list (h:t) = sd' (succ x) (zipWith (+) list h) t

{--
Okay, we've got clusters and cluster centers, now an epoch is to reshuffle the
clusters so that the cells now belong to the clusters with the closest cluster
centers. So we need a distance calculator on cells
--}

sqdist :: (Ix b, Fractional c) => ScoreCard a b c -> Center b c -> c
sqdist (SC _ val) = uncurry sqDist . curry (adjoin elems) val

-- the actual distance is the sqrt (Σ (distance diffs squared)) but as we can
-- blithely, for now, ignore scaling, we can (for now) dispense with the sqrt
-- and just compute (then compare) square distances. May have down-the-road
-- consequences...

sqDist :: Fractional ä => [ä] -> [ä] -> ä
sqDist = sum <<- zipWith ((^2) <<- (-))

-- so with the sqdist we can return the cluster ID on a cell closest to its center

closest :: (Ord ä, Fractional ä, Ix b) =>
           [(Int, Center b ä)] -> ScoreCard a b ä -> Int
closest ((idx, ctr):rest) sc = fst (c' (idx, sqdist sc ctr) rest sc)

c' :: (Ix b, Ord ä, Fractional ä) =>
          (Int, ä) -> [(Int, Center b ä)] -> ScoreCard a b ä -> (Int, ä)
c' ans [] _ = ans
c' base@(i, dist) ((idx, ctr):rest) sc =
   let newdist = sqdist sc ctr
   in  c' (if newdist < dist then (idx, newdist) else base) rest sc

-- so: epoch

epoch :: (Ix b, Ord c, Fractional c) => SCClusters a b c -> SCClusters a b c
epoch clusters =

-- for each cluster, we compute the cluster center
   let ccs = map (second computeClusterCenter) (Map.toList clusters)

-- then for each cell, we assign it to the cluster whose center is closest
       pool = dlToList (mconcat (map snd $ Map.elems clusters))
   in  foldr (\val -> add1 (closest ccs val) val) Map.empty pool

-- now all that remains the start and the end.

-- the start: divy up the pool into n clusters

divy :: Int -> [ScoreCard a b c] -> SCClusters a b c
divy n = flip (d' n) Map.empty . splitAt n

d' :: Int -> ([ScoreCard a b c], [ScoreCard a b c])
          -> SCClusters a b c -> SCClusters a b c
d' n (sommat, []) = addn sommat
d' n (sommat, rest) = d' n (splitAt n rest) . addn sommat

addn :: [ScoreCard a b c] -> SCClusters a b c -> SCClusters a b c
addn sommat = flip (foldr (uncurry add1)) (zip [1..] sommat)

-- the end: stop when the clusters become stable.

sameCluster :: Eq a => SCCluster a b c -> SCCluster a b c -> Bool
sameCluster (sz1, cl1) (sz2, cl2) = 
   sz1 == sz2 && dlToList cl1 == dlToList cl2

-- and now we tie it all together:

kmeans :: (Eq a, Ix b, Ord c, Fractional c) =>
          Int -> [ScoreCard a b c] -> (Int, SCClusters a b c)
kmeans = km' 0 <<- divy

km' :: (Eq a, Ix b, Ord c, Fractional c) =>
       Int -> SCClusters a b c -> (Int, SCClusters a b c)
km' gen start =
   let next = epoch start 
   in  if   all (uncurry sameCluster)
                (uncurry zip (adjoin Map.elems (start, next)))
       then (gen, start) else km' (succ gen) next

{-- So.

*Graph.KMeans> loadScoreCardsFromEndpoint (endpoint ++ ('/': transaction)) queryTop5shows ~> scs ~> head
SC {idx = AA, values = [(Count,35.0),(Min,1.0),(Max,34.0),(Mean,8.8529415),(StdDev,0.35714287)]}

*Graph.KMeans> let (gens, fivers) = kmeans 5 scs
*Graph.KMeans> gens ~> 34

*Graph.KMeans> instance Show (DList a) where show _ = "some dlist"

A default representation for showing dlists just so we can see the sizes of the
clusters.

*Graph.KMeans> fivers ~> 
fromList [(1,(Sum {getSum = 36},some dlist)),
          (2,(Sum {getSum = 13},some dlist)),  <-- interesting
          (3,(Sum {getSum = 110},some dlist)),
          (4,(Sum {getSum = 50},some dlist)),
          (5,(Sum {getSum = 128},some dlist))]

hm, just with 5 clusters we have an interesting isolation in cluster 2 with 13 cells

*Graph.KMeans> let (gens, tenners) = kmeans 10 scs ~> gens ~> 22
*Graph.KMeans> tenners ~>
fromList [(1,(Sum {getSum = 13},some dlist)),(2,(Sum {getSum = 36},some dlist)),
          (3,(Sum {getSum = 119},some dlist)),(4,(Sum {getSum = 49},some dlist)),         (8,(Sum {getSum = 11},some dlist)),(10,(Sum {getSum = 109},some dlist))]

Wow! K-means got uninterested! It says there are only six defined clusters!
That is neat! So we don't need bother with the 30 clusters option. Cool!

Then, when you've created your kmeans clustering algorithm, as yesterday,
read in the stock set of score cards either from the graph DaaS endpoint
or from CSV file, e.g.: http://lpaste.net/raw/3755904211217285120 then cluster
these ScoreCards. Try different numbers of clusters, like, I don't know: 5, 10,
and 30 clusters. How many score cards do you have in each of the resulting
clusters? Which clustering-scheme makes most ... 'sense'?

The last question may be hard to answer without context. Next week we will
enhance these (meta-)data sets to provide views that may give context.

And, hey, just gap analysis? Is that sufficient? Well, yes, for some analyses.
But we can get more information given a stock symbol, e.g. its price (open,
close, high, low) for the day or days, its market capitalization, its volume,
its beta, its eps, its p/e ('price to earnings'), it div/yield, its shares ...

This additional information can be extracted or scraped from various financial
sources. We may enhance our score cards with these additional data points to
look at different, hopefully better, clustering results.
--}
