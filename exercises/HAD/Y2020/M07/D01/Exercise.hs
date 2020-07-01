module Y2020.M07.D01.Exercise where

import Data.Graph

{--
Okay, yesterday we converted a highly connected, cyclical set of arcs. Today
we'll look at pathing through a graph's nodes, but let's do this with a simpler,
not-so-connected, graph for today's exercise.

Find all acyclic* paths from node a to node b in the graph g.

* acyclic in that a node is visited only once in the path.
--}

data Node = R | S | T | U | V
   deriving (Eq, Ord, Show)

data Arc = Arc Node Node        -- no distance-edges in this graph
   deriving (Eq, Ord, Show)

graphArcs :: [Arc]
graphArcs =  [Arc S R, Arc S U, Arc U S, Arc U R, Arc V U]

-- n.b.: node T is not connected to any other node

graph :: [Arc] -> Graph   -- should be same-..ish as yesterday
graph arcs = undefined

path :: Graph -> Node -> Node -> [[Arc]]
path g a b = undefined
