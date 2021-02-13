{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D05.Solution where 

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

import Y2021.M02.D04.Solution (dupes, NodeIds)
import Y2021.M01.D21.Solution (Idx)

import Graph.Query (getGraphResponse, graphEndpoint, Endpoint)
import Graph.JSON.Cypher (Cypher)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

removeDuplicateWines :: Endpoint -> NodeIds -> IO String
removeDuplicateWines url =
   getGraphResponse url
      . map (removeWinesQuery . tail . Set.toList)
      . Map.elems

{--
-- what removeDuplicateWines does is, for the first wine id (the head of the
-- list), we remove the relation:

removeRelationQuery :: Idx -> Cypher
removeRelationQuery wineId =
   T.pack (unwords ["MATCH (w:Wine)<-[r:RATES_WINE]-()",
                    "WHERE id(w) =", show wineId, "DELETE r"])

Actually, we don't need to do this if we remove all (wine)<-[:RATES_WINE]-(t)
relations in the database with:

neo4j$ match (:Wine)<-[r:RATES_WINE]-(:Taster) delete r

Deleted 119981 relationships, completed after 2288 ms.

This further means that we only have to work with the duplicates, not with
the entire wine-set.
--}

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

{--
>>> graphEndpoint 
...
>>> let url =it
>>> :set -XOverloadedStrings 
>>> dupes url "Wine" "title"
...
>>> let vinos = it
>>> let vino = Map.fromList . return . head $ Map.toList vinos
>>> removeDuplicateWines url vino
"{\"results\":[{\"columns\":[],\"data\":[]}],\"errors\":[]}"

Boom, we go from vinos to vino, removing the duplicates for one wine.

That was easy. Now: 10 wines:

>>> let vino = Map.fromList . take 10 . tail $ Map.toList vinos
>>> removeDuplicateWines url vino
...

also instantaneous.

>>> let vinos1 = Map.fromList . drop 11 $ Map.toList vinos
>>> Map.size vinos1
923

We have a ways to go. Let's try 50?

>>> let vino = Map.fromList . take 50 $ Map.toList vinos1
>>> removeDuplicateWines url vino

That took a second.

How about 100?

>>> let vinos2 = Map.fromList . drop 50 $ Map.toList vinos1
>>> let vino = Map.fromList . take 100 $ Map.toList vinos2
>>> removeDuplicateWines url vino
...

Barely any delay at all. 200?

>>> let vinos3 = Map.fromList . drop 100 $ Map.toList vinos2
>>> let vino = Map.fromList . take 200 $ Map.toList vinos3
>>> removeDuplicateWines url vino
...

Quick! ... 500? ... wait. What's the size now?

>>> let vinos4 = Map.fromList . drop 200 $ Map.toList vinos3
>>> Map.size vinos4
573

Well, well, well. How about 573, then.

>>> removeDuplicateWines url vinos4
...

... annnnnnddddd, ... we're done? Let's check.

>>> dupes url "Wine" "title"
fromList []

Yup! We're done.

... now: how about Wineries?

>>> dupes url "Winery" "name"
fromList []

Cool beans! Good night!
--}
