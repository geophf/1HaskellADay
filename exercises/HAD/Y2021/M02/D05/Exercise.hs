{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D05.Exercise where 

{--
Okay, yesterday we found lots of duplicates of wine-labels and, digging further,
we saw some wines had multiple reviews from the same reviewer, but also, some
wines had multiple reviews from multiple reviewers.

This begs the question, however: so?

Why do I say this?

Well, down the road (not today), because a wine may have multiple reviews from
a reviewer, that means there are going to be multiple RATES_WINE relations
between the same two nodes (wine, taster), and to CREATE multiple relations,
we cannot add to one relation, no: we have to create a new relation for each
review.

Which means, in which case, that we want all the current relations to go away,
so that we can add relations between wines and tasters as we add each review
(and rating ... and possibly price).

So: there 'tis.

What does that mean for us now?

What that means, tactically-speaking, is that for each wine with wine ids, (h:t)

* for h, we delete the relation r of (wine { id: h })-[r]-(taster); and,
* for a ids in t, we detach delete w (the redundant wine).

We do this for every wine in our graph-store, we then should have a newly-
cleaned state by which we can (re-)start uploading reviews to our graph.

Let's do it.

Yesterday we got a mapping of all wines to their (possibly multiple) ids:
--}

import Y2021.M02.D04.Solution (nodeMap, NodeIds)
import Y2021.M01.D21.Solution (Idx)

import Graph.Query
import Graph.JSON.Cypher

import qualified Data.Text as T

removeDuplicateWines :: Endpoint -> NodeIds -> IO String
removeDuplicateWines url wines = undefined

-- what removeDuplicateWines does is, for the first wine id (the head of the
-- list), we remove the relation:

removeRelationQuery :: Idx -> Cypher
removeRelationQuery wineId =
   T.pack (unwords ["MATCH (w:Wine)<-[r:RATES_WINE]-()",
                    "WHERE id(w) =", show wineId, "DELETE r"])

-- For the rest of the wine ids (the rest of the wine ids for a wine), we want 
-- to detach and delete those wines, because they are duplicates.

removeWinesQuery :: [Idx] -> Cypher
removeWinesQuery [] = ""
removeWinesQuery l@(_:_) =
   T.pack (unwords ["MATCH (w:Wine) WHERE id(w) IN", show l,"DETACH DELETE w"])

-- Remove all the duplicate wines and all RATES_WINE relations. You may want 
-- to try this on one wine, then 10 wines, then 100 wines, ... just to see how 
-- work flows.

-- Verify, after running removeDuplicateWines, that there are, in fact, no
-- duplicate wines remaining in the graph store.

-- When you verify no dupes, run an index on the Wine.title.
