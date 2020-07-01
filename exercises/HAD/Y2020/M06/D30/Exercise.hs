module Y2020.M06.D30.Exercise where

import Data.Graph

{--
Let's say we have a graph (see graph.png, this directory) ... oh, okay: here
are the arcs of the graph
--}

data Node = A | B | C | D | E | F | G | H
   deriving (Eq, Ord, Show)

data Arc = Arc Node Node Int
   deriving (Eq, Ord, Show)

graphArcs :: [Arc]
graphArcs = 
   let arcs = zipWith id (zipWith Arc [A,A,B,B,C,D,D,D,E,F,G]
                                      [B,D,C,E,E,E,F,G,H,G,H])
                  [5,3,2,4,6,7,4,3,5,4,1]
   in  arcs ++ map flipNodes arcs
      where flipNodes (Arc a b c) = Arc b a c

{--
We're going to be studying paths, cycles, and spanning trees of graphs, but to
do that, we need to represent the above set of arcs as a graph.

So, for today's exercise, convert the above arcs to a graph data type. Now,
you represent your graph howmst'd'soever you choose to represent your graph,
but might I suggest the graph-representation from Data.Graph?

Yes, I mightest.
--}

graph :: [Arc] -> Graph
graph arcs = undefined
