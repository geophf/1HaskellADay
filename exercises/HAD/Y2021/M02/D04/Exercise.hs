{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D04.Exercise where

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

import Data.Aeson.WikiDatum (Name)

import Graph.Query

import Data.Map (Map)
import Data.Set (Set)

{--
... but this time, instead of returning a map that overwrites duplicates,
let's return a map that enumerates them:
--}

type NodeIds = Map Name (Set Idx)

nodeMap :: Endpoint -> Name -> Name -> IO NodeIds
nodeMap url nodeName attribName = undefined

{--
With this, above, how many, exactly wine-label duplicates are there? Are
there triplicates? sextuplets? megalons?

... wait ... wut?

Okay, now that we have our duplicates, let's examine their structures and
see how we can start to consolidate them. ... We'll do this tomorrow.
--}
