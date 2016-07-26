module Y2016.M07.D26.Exercise where

{--
So, yesterday's solution showed us that we can path through the graph, reaching
any node that we could reach in the figure. YAY!

Problem here. The paths weren't necessarily the shortest path nor were they the
straightest path.

Well, what is 'shortest path'?

One way to look at shortest path is 'least number of nodes visited.' 

Yesterday's solution fails this test: the path from 'a' to 'c' was given as:

"abjfkgmhnc"

Now that is, indeed, a path: it gets us from node a to node c, however, this is
a very round-about way to get there: a direct path would, eyeballing the figure:

"abgc"

Why is this round-about answer given? How do we fix this?

Today's Haskell exercise: discover the shortest path between two nodes, 
'shortest,' in this case, meaning 'least number of nodes visited.'

Remember to take into account cycles, if they are there any.
--}

import Y2016.M07.D20.Solution (FigureC)

leastVerticesPathing :: FigureC -> Char -> Char -> [String]
leastVerticesPathing fig start end = undefined

{--
The returned value: there may be multiple least-vertices pathings through a
graph.

Note the care in the name of this function. Least nodes visited is not 
necessarily the shortest path, looking at the figure (for example, yesterday's
'm' to 'a' path was "mgba" which is, distance-wise, not the shortest, even though
four nodes is the (tied) least number of nodes. We will look at adding distance
information for shortest distance path tomorrow.
--}
