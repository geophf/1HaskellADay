module Y2016.M07.D25.Exercise where 

{--
It turns out that counting trigons ('triangles') in a figure is not a trivial
task! We haven't even gotten to the point where we can do that, because a graph
of complete information of the figure contains cycles, so simply pathing in this
graph is non-trivial!

Pathing through an acyclic graph is (theoretically) trivial, but that's for the
case for the acyclic graph containing complete information. We haven't (yet)
encountered that acyclic graph, so let's, today, focus on solving the pathing-
problem for the cyclic graph.

How do we path through a cyclic graph?

One way to do this, if you know a priori that you need only touch nodes at most
one time only, it to memoize the path taken. And one way to do that is to
collect nodes visited into a Set.

For each new node attempted, you check the visited-set, and if that potential
node is in the visited-set, you know you aren't going to go that way (again).

This may become cumbersome 'in the large,' ... how many nodes? 1000s? or orders
of magnitude larger? But usually I deal with 'small' graphs then: 2000 nodes
or so (common English words, for example, or top5s stock symbols).  The graphs
we are dealing with here are even smaller than that: 10s of nodes.

It'd be an interesting study to see how this pans out in the large.

Maybe somebody with some (real) big data (as opposed to fake big data) (or 'big
data' that idiots call big data. "We have three gigabytes of data, so that's big!"

Uh, no, it's not. Big data is not what a laptop can manage, mkay?)

At any rate.

Today's Haskell exercise. Pathing in a cyclic graph, use a set to collect
already visited nodes.

--}

import Y2016.M07.D22.Solution (graphit, pathing)
import Y2016.M07.D20.Solution (FigureC, lineSegments)
import Y2016.M07.D19.Exercise (figure2)

acyclicPathing, cyclicPathing :: FigureC -> Char -> Char -> String
acyclicPathing = pathing


cyclicPathing fig start end = undefined

-- hint: cyclicPathing may use a helper function that uses a Set Char (nodes
-- already visited) to disallow cycles in sought paths.

-- Same questions from last week for today's exercise. CAN WE ANSWER THEM?!?

-- Now with the new, correct, definitions, answer these pathing problems:

-- 1. what is the path from 'a' to 'c'?
-- 2. what is the path from 'b' to 't'?
-- 3. what is the path from 'b' to 'h'?
-- 4. what is the path from 'd' to 'f'?
-- 5. what is the path from 'm' to 'a'?

-- As before:

-- let fig@(gr,fnPt,fnVertM) = graphit figure2 lineSegments 
-- gets you the figure and

-- Data.Graph.dfs gr [0] is the Forest of Vertex-values from 0 vertex (which is,
-- as you recall, point 'a' in the figure)
