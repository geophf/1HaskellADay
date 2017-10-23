{-# LANGUAGE OverloadedStrings #-}

module Data.Hierarchy where

-- An Hierarchy is an n-tree, useful for describing rooted hierarchies

import Control.Arrow (second, (***), (&&&))
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid

{--
Below imports demonstrate circle-packing of clusters for top5s stocks:

-- import Graph.ScoreCard.Loader
-- import Graph.Top5sAppearances (queryTop5shows)
--}

{-- A solution to the problem posted at http://lpaste.net/224067301371019264
@1HaskellADay solution for 2016-05-02

ref: Graph/d3circs.hs

Possible imports that may help:

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

The Hierarchy value can be encoded as a JSON-string

e.g.:

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

-- below import available via 1HaskellADay git repository

import Data.Relation

data Hierarchy a = Hier { node :: a, kids :: Children a } deriving Show
data Children a = Kids [Hierarchy a]
              | Size Int
   deriving Show

class Tuple a where
   toPair :: a -> Pair

instance Tuple a => ToJSON (Hierarchy a) where
   toJSON (Hier nm (Kids k)) = object (toPair nm:kiddies k)
   toJSON (Hier nm (Size s)) = object [toPair nm, "size" .= s]

kiddies :: Tuple a => [Hierarchy a] -> [Pair]
kiddies [] = []
kiddies k = ["children" .= map toJSON k]

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

{--
-- okay, so what data is hierarchical?

class Hierarchical a where
   hierarchy :: a -> Hierarchy

-- we can also make an hierarchy a set of relations

   relation :: a -> [Relation b rel b]
   nodify :: a -> b

instance Hierarchical Hierarchy where
   hierarchy = id

-- the hierarchy of a hierarchy is itself
-- [so, I guess we're in the hierarchy category now]

   relation (Hier nm (Kids k)) =
      map (Rel (BRANCH nm) CONTAINS . nodify) k ++ concatMap relation k
   relation (Hier nm (Size sz)) = []

   nodify (Hier nm (Size sz)) = LEAF nm sz
   nodify (Hier nm (Kids k)) = BRANCH nm

-- the default relationship in an hierarchy is containment

data Container = CONTAINS
   deriving (Eq, Show)

instance Edge Container where asEdge = show

-- the default nodes of a hierarchy are branching nodes and leaf nodes

data Branch = BRANCH String | LEAF String Int
   deriving (Eq, Show)

instance Node Branch where
   asNode (BRANCH nm) = "BRANCH { name: '" ++ nm ++ "' }"
   asNode (LEAF nm sz) = "LEAF { name: '" ++ nm ++ ", size: " ++ show sz ++ " }"

-- and the answer is no ... until I can master dependent types in Haskell
--}

-- Okay, the above was me trying to generalize to the type-level hierarchies-
-- as-relations. That's a problem (that Haskell has solved, I'm sure), but a
-- much simpler is 'relational'-izing, specifically, Hierarchy, or, generally
-- Hierarchy a where a is some relatable-type.

-- figuring out what a 'relatable-type' is an interesting problem, too.

-- So, is there some type, Hierarchy a, where a is relatable, such that
-- Hierarchy a is relatable?

-- Well, yes. As plain Hierarchy, under the old regime, or, now:
-- Hierarchy String is relatable, then if a is hierarchical then a is relatable

-- Q.E.D.

-- Now, with the redefinition of Hierarchy (string) to Hierarchy a where a
-- is a tuple-type, we can relationalize an hierarchy by treating the tuple
-- as a typed value where the key is the name of the type

-- ... or, more generally: we can realize relations from any hierarchy
-- of relatable types thus:

hier2rel :: Relatable a a rel => Hierarchy a -> [Relation a rel a]
hier2rel (Hier _ (Size _)) = []
hier2rel (Hier h (Kids kids)) = 
   zipWith relate (repeat h) (map node kids) ++ concatMap hier2rel kids

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
