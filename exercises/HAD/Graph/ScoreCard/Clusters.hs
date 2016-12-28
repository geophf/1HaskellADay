{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TupleSections #-}

module Graph.ScoreCard.Clusters where

-- http://lpaste.net/8982640331094753280

import Control.Arrow ((>>>), (&&&), second)
import Data.Array
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid

-- below imports available from 1HaskellADay git repository

import Data.Graphics.Color
import Data.Relation
import Graph.JSON.Cypher
import Graph.KMeans
import Graph.Query
import Graph.ScoreCard
import Graph.ScoreCard.Colored
import Graph.ScoreCard.ColorScheme
-- import Graph.Top5sAppearances      -- http://lpaste.net/4928100283508064256

{-- A solution to the problem posted at http://lpaste.net/9073884707980050432
@1HaskellADay solution for 2016-04-12

Okay, yesterday we got the data cells all labeled and compacted on a grid ... Great!

Now, let's look at these data in a different way.

Cluster the score cards using the K-means algorithm, imported above. Great, now you
have a set of clusters (also known as a 'mapping' from cluster ids to clustered values)

Now, represent the clusters as sets of relations.
--}

data NamedClusters key scorecard = NC { tup :: (String, Clusters key scorecard) }
   deriving Show

instance Node (NamedClusters key scorecard) where
   asNode (NC (nm, _)) = "CLUSTERS { for: " ++ show nm ++ ", name: 'Clusters' }"

colorization :: (Ix a, Ix b, Ord key, RealFrac c) => SCClusters key a b c
             -> [ScoreCard a b c]
             -> Clusters key (ColoredScoreCard a b c)
colorization clusters origs =  -- ...

{--
Actually, this isn't as easy as adding 'COLOR' index, right? ... or I don't
see an easy way to add a type-value for a sum-type. So, do we add a property
to ScoreCard? Hm.

Well, first of all, we colorize our scorecards. Easy enough.
--}

   let colours = colorize (mconcat (map snd (Map.elems clusters)))
       scores  = array (bounds colours) (map (idx &&& id) origs)
       mkCSC   = uncurry CSC . ((scores !) &&& colorIdx . (colours !)) . idx
   in  Map.map (second (fmap mkCSC)) clusters

data Membership = IN deriving Show

instance Edge Membership where asEdge = show

data CSCCluster key a b c = CSCC key (Cluster (ColoredScoreCard a b c))

instance Show key => Node (CSCCluster key a b c) where
   asNode (CSCC idx clust@(Sum x, cells)) =
      let color = getSum (foldMap (Sum . colour) cells) `div` x in
      "CLUSTER { idx: \"" ++ show idx ++ "\", heat: " ++ showColor color
              ++ ", size: " ++ show x ++ " }"

type CSCCRel key a b c = Relation (Src key a b c) Membership (Src key a b c)

relateClusters :: (Enum a, Ix a, Ix b, RealFrac c) =>
        NamedClusters key (ColoredScoreCard a b c) -> [CSCCRel key a b c]
relateClusters =
   uncurry concatMap . (relateCluster &&&
                        map (uncurry CSCC) . Map.toList . snd . tup)

data Src key a b c = Root (NamedClusters key (ColoredScoreCard a b c))
                   | Branch (CSCCluster key a b c)
                   | Leaf (ColoredScoreCard a b c)

instance (Show a, Show b, Ix b, Show c, Show key) => Node (Src key a b c) where
   asNode (Root x)   = asNode x
   asNode (Branch x) = asNode x
   asNode (Leaf x)   = asNode x

instance (Show a, Ix b, Show b, Show c, Show key) => Show (Src key a b c) where
   show = asNode

relateCluster :: Enum a => NamedClusters key (ColoredScoreCard a b c)
              -> CSCCluster key a b c -> [CSCCRel key a b c]
relateCluster c cluster@(CSCC _ (_, cells)) = 
   Rel (Root c) IN (Branch cluster):map (relateCSC cluster) (toList cells)

relateCSC :: CSCCluster key a b c -> ColoredScoreCard a b c -> CSCCRel key a b c
relateCSC cluster cell = Rel (Branch cluster) IN (Leaf cell)

{--
*Main> let rels = relateClusters scores clusters
*Main> uploadClusters (endpoint ++ ('/': transaction)) rels

Resulting graph with nodes enhanced with meta-data shown on @1HaskellADay
--}

{-- BONUS --------------------------------------------------------------------

Upload these relations to a graph DaaS; show your results as a graph.

... n.b.: If you wish to relate clustered cells to the original data set, see
relateClusteredCells below.

--}

uploadClusters :: (Foldable t, Functor t, Node a, Edge b, Node c) =>
                  Endpoint -> t (Relation a b c) -> IO String
uploadClusters endpt =
   getGraphResponse endpt . fmap (mkCypher "src" "rel" "dest")

{--
*Main> uploadClusters (endpoint ++ ('/': transaction)) rels ~> 
{..., "errors": [] }

No errors. Good! Cluster samples shown on @1HaskellADay
--}

-- Okay, next step: relate the clustered cells to the original data set:

{-- A solution to the problem posted at http://lpaste.net/5271793028346937344
@1HaskellADay solution for 2016-04-15

ref: Graph/relate.hs

So, yesterday, you scaled and colored Scorecards, giving context to meta-data
(which gives context to the underlying data, hint-hint). Today, let's tie it
all together.

As before load in the scorecards from the endpoint (which is actually the 
process of loading and transforming the data, hint-hint), scale, color, and
cluster these (meta-)data, and load the clusters to the graph DaaS endpoint.

... there's a lot of hint-hint-ing going on here.. can you say: Foreshadowing?

Now, today: link the colored, clustered scorecards back to the source data,
so you can look at the source data, for further exploration.
--}

data Linkage = LINKS_TO deriving Show

instance Edge Linkage where asEdge = show

data SimpleCell a = Cell a deriving Show

instance Show a => Node (SimpleCell a) where
   asNode (Cell s) = "CELL { idx: \"" ++ show s ++ "\" }"

data SimpleDatum a = Datum a deriving Show

instance Show a => Node (SimpleDatum a) where
   asNode (Datum s) = "DATUM { symbol: \"" ++ show s ++ "\" }"

mkRel :: Show a => ColoredScoreCard a b c
      -> Relation (SimpleCell a) Linkage (SimpleDatum a)
mkRel = (Cell &&& Datum >>> uncurry (`Rel` LINKS_TO)) . idx . sc

relateClusteredCells :: Show a => Endpoint
                     -> NamedClusters z (ColoredScoreCard a b c) -> IO String
relateClusteredCells endpt =
   uploadClusters endpt . fmap mkRel . mconcat
                        . Map.elems . Map.map snd . snd . tup

{--
*Main> loadScoreCardsFromEndpoint (endpoint ++ ('/': transaction)) queryTop5shows ~> scores
*Main> let scaled = scaleScores scores 
*Main> let (itrs, clusters) = kmeans 10 scaled
*Main> let nms = NC ("Top5s", colorization clusters scores )
*Main> relateClusteredCells (endpoint ++ ('/': transaction)) nms
...,{\"columns\":[],\"data\":[]}],\"errors\":[]}"

YAY! No errors in upload!

-- This will be rolled into the graph scorecard clustering modules.
--}
