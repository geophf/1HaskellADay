module Y2020.M07.D06.Exercise where

import Data.Graph

{--
Today we're going to extract the spanning trees from a graph. 'What's a spanning
tree?' you ask.

WIKIPEDIA TO THE RESCUE!

https://en.wikipedia.org/wiki/Spanning_tree

In the mathematical field of graph theory, a spanning tree T of an undirected 
graph G is a subgraph that is a tree which includes all of the vertices of G, 
with a minimum possible number of edges. In general, a graph may have several 
spatnning trees, but a graph that is not connected will not contain a spanning 
tree (but see Spanning forests below). If all of the edges of G are also edges 
ofa spanning tree T of G, then G is a tree and is identical to T (that is, a 
tree has a unique spanning tree and it is itself).

So there it is!

Today, let's extract the spanning trees from an undirected graph. Which means
we build an undirected graph from a set of edges, first!
--}

data Node = A | B | C | D | E | F | G | H
   deriving (Eq, Ord, Show, Enum)

data Edgy = Eg Node Node
   deriving (Eq, Ord, Show)

undirectedEdges :: [Edgy]
undirectedEdges = [Eg A B, Eg A D, Eg B C, Eg B E, Eg C E, 
                   Eg D G, Eg E H, Eg F G, Eg G H, Eg D E, Eg D F]

graphFromEdges :: [Edgy] -> Graph
graphFromEdges egs = undefined

-- now that we have the graph, let's get the spanning trees

data SpanningTree = Path [Edgy]
   deriving (Eq, Ord, Show)

spanningTrees :: Graph -> [SpanningTree]
spanningTrees gr = undefined
