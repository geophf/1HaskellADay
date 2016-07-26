module Y2016.M07.D27.Exercise where

{--
One definition of 'shortest path' is least number of nodes visited. That's one
definition, but that's not the only definition, nor is the most-complete based
on the information provided by the figure, for 'shortest path' from nodes 'm'
to 'a' given in yesterday's solution was: mgba, 4 nodes. But is that the shortest
path, looking at the figure (look at the figure on Y2016/M07/D19/tri2.gif)

No. The distance traversed by that path is:

m - 10 -> g - 10 -> b - 18.03 -> a

for a total distance of 38.03

But, eyeballing the figure, there is an obviously shorter (shortest) path: mkja

m - 5 -> k - 5 -> j - 15 -> a

for a total distance of 25

A much smaller distance. This also applies for the leastVerticesPathing, a -> c
path leastVerticesPathing is also the shortestDistancePathing but this is not
necessarily the case based on the graph.

We need to enhance the graph to include a function that gives the distance 
between any two nodes.
--}

import Y2016.M07.D19.Solution (figure2)
import Y2016.M07.D20.Solution (FigureC, lineSegments)
import Y2016.M07.D22.Solution (graphit)

distance :: FigureC -> Char -> Char -> Float
distance fig start end = undefined

-- hint, as the node contains its location in cartesian space, then ... yeah.

shortestDistancePathing :: FigureC -> Char -> Char -> String
shortestDistancePathing fig start end = undefined

-- Now, how we go about finding the shortest path ... parallelism? ACO?
-- AlphaBeta? A*? Genetic Algorithm? That's up to you. There are papers on this,
-- I think. Hint: leastVerticesPathing is not necessarily shartestDistancePathing
-- for any graph g. A graph can have 10 nodes, each of distance 1, so the 
-- leastVerticesPathing may have a three node traversal, but if each of those
-- nodes is of distance 20, then the shortestPath would be those 10 nodes of
-- much smaller distance. There is no correllation between shortest distance
-- and least vertices in the general case.

-- So, with shortestDistancePathing defined, fine the shortest distance between
-- nodes 'm' and 'a' in the figure from the value of:

-- let fig = graphit figure2 lineSegments
