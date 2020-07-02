module Y2020.M07.D02.Exercise where

import Data.Graph

{--
Okay, recall that highly connected graph from earlier this week?
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

graph :: [Arc] -> Graph
graph arcs = undefined

{--
So, today, we're going to be studying cycles in graphs, ... MOTORCYCLES! AHA!

... no, ... wait ...

With the function cycles, this function will return all cycles for node a,
BUT NOT! ... and this is important, BUT NOT! ... fall into an infinite loop
of cycles at the get go and give you nothing in return.

Ooh! Salty cycles need to be ... um ... desalinated?

How does one 'unsalt' something?

Problem! (edited from p83 of the Prolog p99 problem set):

Write a function cycles to find the closed paths (cycles) p starting at a given 
node a in the graph g.

We are not considering lengths of arcs with today's problem. We'll look at 
lengths another day when we compute minimal spanning tree.
--}

cycles :: Graph -> Node -> [[Arc]]
cycles gr a = undefined
