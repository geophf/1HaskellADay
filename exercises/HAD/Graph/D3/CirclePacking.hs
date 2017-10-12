{-# LANGUAGE OverloadedStrings #-}

module Graph.D3.CirclePacking where

import Control.Arrow (second, (***), (&&&))
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid

-- Below imports available via 1HaskellADay git repository

import Data.Hierarchy

import Graph.KMeans
import Graph.Query
import Graph.ScoreCard
import Graph.ScoreCard.Clusters
import Graph.ScoreCard.Colored

{--
Below imports demonstrate circle-packing of clusters for top5s stocks:

-- import Graph.ScoreCard.Loader
-- import Graph.Top5sAppearances (queryTop5shows)
--}

{-- A solution to the problem posted at http://lpaste.net/224067301371019264
@1HaskellADay solution for 2016-05-02

ref: Graph/d3circs.hs

Possible imports that may help:

import Control.Arrow
import Data.Array
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid

import Control.List (weave)
import Data.Graphics.Color
import Data.Relation
import Graph.JSON.Cypher
import Graph.ScoreCard.ColorScheme

GOOD MORNING AND HAPPY MAY!

So, today's problem. See the circles on http://bl.ocks.org/mbostock/4063530

If you look at the JSON, it falls into a simple scheme of

{ name: "[something]"
  children: [
    <name then name|children>
  ]
}

Today, take the cluster we generated from the April 12th problem:

http://lpaste.net/9073884707980050432

Or take a clustered or hierarchical data set that you have and output those
data as the above name|children JSON format.
--}

relatedDataAsPackedCircles :: (Ix b, Show a) => b -> NamedClusters Int (CSC a b) -> Hierarchy
relatedDataAsPackedCircles = asHier

-- the Hierarchy value can be encoded as a JSON-string

{--
*Main> loadScoreCardsFromEndpoint (endpoint ++ ('/': transaction)) queryTop5shows 
[SC AA [(Count,35.0),(Min,1.0),(Max,34.0),(Mean,8.8529415),(StdDev,8.4)],
 SC AAPL [(Count,171.0),(Min,1.0),(Max,10.0),(Mean,1.882353),(StdDev,1.5333333)],...]
*Main> let scaled = scaleScores scores
*Main Graph.KMeans> let (itrs, clusters) = kmeans 10 scaled

With the clusters you can get the name|children circles, but we want the
counts in the subcircles and this we get from the original scores so:

*Main Graph.KMeans> let nms = NC ("Top5s", colorization clusters scores )

With this we can now generate our JSON hierarchical structure
--}

type CSC a b = ColoredScoreCard a b Float
type ClCSC a b = Cluster (ColoredScoreCard a b Float)

asHier :: (Ix b, Show a) => b -> NamedClusters Int (CSC a b) -> Hierarchy
asHier b = uncurry Hier . second (Kids . map (toHier b) . Map.toList) . tup

toHier :: (Ix b, Show a) => b -> (Int, ClCSC a b) -> Hierarchy
toHier b = uncurry Hier . (show *** kidsFrom b)

kidsFrom :: (Ix b, Show a) => b -> ClCSC a b -> Children
kidsFrom b = Kids . map (hierFromSc b . sc) . toList . snd

hierFromSc :: (Ix b, Show a) => b -> ScoreCard a b Float -> Hierarchy
hierFromSc b = Hier . show . idx <*> Size . floor . (! b) . values

{--
Whew! That only took a month! SO!
*Main> loadScoreCardsFromEndpoint (endpoint ++ ('/': transaction)) queryTop5shows ~> scores
*Main> let scales = scaleScores scores
*Main> let (itrs, clusters) = kmeans 10 scales ~> 33 iterations
*Main> let colored = colorization clusters scores
*Main> mapM_ print (Map.toList colored )
(2,(Sum {getSum = 114},a DList))
(3,(Sum {getSum = 26},a DList))
(4,(Sum {getSum = 34},a DList))
(5,(Sum {getSum = 15},a DList))
(6,(Sum {getSum = 11},a DList))
(7,(Sum {getSum = 118},a DList))
(10,(Sum {getSum = 34},a DList))

... so here we go!
*Main> let nms = NC ("Top5s", colored)
*Main> let hier = asHier Count nms
*Main BL> encode hier ~> "{\"children\":[{\"children\":[{\"name\":\"GMCR\","size":12},...
*Main BL> BL.writeFile "Graph/top5s.json" (encode hier)
--}

{-- BONUS ------------------------------------------------------------------

Use d3js.org or another data-representation framework to show the results
of your data transformation. Share on twitter!

You can run a web-server with, e.g.:

$ php -S 127.0.0.1:8080

At the directory where your circles.html and top5s.json is located

A sample circle-packing algorithm against D3js.org can be found at:

http://bl.ocks.org/mbostock/4063530

or, here: Graph/D3/circles.html
--}
