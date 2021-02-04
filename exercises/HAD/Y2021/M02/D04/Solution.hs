{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D04.Solution where

{--
"A funny thing happened to me on my way to uploading reviews ..."

So, yesterday, we started uploading reviews, but then something funny in the
graph data set bugged me:

>>> Map.size winy
118840

But when I query the graph data store I get:

neo4j$ match (t:Wine) return count(t)

count(t)
119988

WHAT? This means there are over 1,000 wine label duplicates. That's less than
one percent duplication, but still: it bugs me.

So, today, we're going to locate the duplicates in the wine graph store.

As you recall, we already have a query that returns wines and their id's:
--}

import Y2021.M02.D03.Solution (nodeMapQuery)
import Y2021.M01.D21.Solution (Idx)

import Control.Map (snarf)

import Data.Aeson.WikiDatum (Name)

import Graph.Query
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ord (Down)

import Data.Set (Set)
import qualified Data.Set as Set

{--
... but this time, instead of returning a map that overwrites duplicates,
let's return a map that enumerates them:
--}

type NodeIds = Map Name (Set Idx)

nodeMap :: Endpoint -> Name -> Name -> IO NodeIds
nodeMap url nodeName attribName =
   RR.multimap <$> getGraphResponse url [nodeMapQuery nodeName attribName]

{--
>>> graphEndpoint 
...
>>> let url = it
>>> :set -XOverloadedStrings 
>>> let wino = nodeMap url "Wine" "title"
>>> Map.size <$> wino
118840
>>> let dupes = Map.filter ((> 1) . Set.size) <$> wino
>>> Map.size <$> dupes
934
>>> dupes
fromList [("1+1=3 NV Brut Sparkling (Cava)",fromList [109356,144120]),
          ("A.R. Lenoble\160 NV Dosage Z\233ro Brut Nature  (Champagne)",fromList [91966,93139,163502]),
          ("A.R. Lenoble\160 NV Grand Cru Blanc de Blancs Chouilly Brut Chardonnay (Champagne)",fromList [112373,116257]),...]
>>> let dupes = it

... to get out of the IO monad.


With this, above, how many, exactly wine-label duplicates are there? Are
there triplicates? sextuplets? megalons?

... wait ... wut?

>>> take 10 . sortOn (Down . snd) . Map.toList $ Map.map Set.size dupes
[("Gloria Ferrer NV Sonoma Brut Sparkling (Sonoma County)",9),
 ("Segura Viudas NV Aria Estate Extra Dry Sparkling (Cava)",7),
 ("Segura Viudas NV Extra Dry Sparkling (Cava)",7),
 ("Bailly-Lapierre NV Brut  (Cr\233mant de Bourgogne)",6),
 ("Gloria Ferrer NV Blanc de Noirs Sparkling (Carneros)",6),
 ("J Vineyards & Winery NV Brut Ros\233 Sparkling (Russian River Valley)",6),
 ("Korbel NV Brut Sparkling (California)",6),("Ruinart NV Brut Ros\233  (Champagne)",6),
 ("Chandon NV Ros\233 Sparkling (California)",5),
 ("Freixenet NV Cordon Negro Extra Dry Sparkling (Cava)",5)]

Oh, yeah. There are some dupes! :<

Okay, now that we have our duplicates, let's examine their structures and
see how we can start to consolidate them. ... We'll do this tomorrow.
--}
