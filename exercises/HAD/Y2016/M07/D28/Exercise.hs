module Y2016.M07.D28.Exercise where

{--
So, we've gotten pathing, non-cycling pathing, least vertices pathing, and
shortest pathing.

What else could we possibly want?

Well, in a 2D Cartesian space, I think we'd want straight pathing, too, because
trigons... which we won't tackle today (the trigons), but we will tackle this:

nodes in a straight line.

Let's tackle it.

Given a graph, g, with nodes that contain cartesian location information, loc,
determine that v1 -> v2 are connected by edges that in a straight line.

Now for the case (a, b), a and b are on a straight line, but how about for the
case (a,b), (b,g), (g,c). Is the line segment that follows those edges on a
straight line?
--}

import Y2016.M07.D19.Solution (figure2)
import Y2016.M07.D20.Solution (FigureC, lineSegments)
import Y2016.M07.D22.Solution (graphit)
import Y2016.M07.D26.Solution (extendPath)

straightLinePathing :: FigureC -> Char -> Char -> [String]
straightLinePathing = undefined

{--
Now, this return-type is interesting, but for some graphs, there may be
multiple straight lines between ('betwixt'? I think 'betwixt' is correct)
v0 and vn, e.g.:

(v0, v1), (v0, v2), (v0, v3), (v2, v4), ... (v0, vn)

If the vs are all along a straight line there may be multiple ways at arriving
at vn besides the (v0, vn) edge.

hm.

And, of course, the value of straightLinePathing fig 'a' 'c' is [], as there is
no figure2 (a,c) path that is along a straight line by following the edges.

So, there's that, too.
--}

-- Question. Given fig = graphit figure2 lineSegments, how many straight 
-- line segments are there? What are they?

allStraightLineSegments :: FigureC -> [String]
allStraightLineSegments = undefined

-- from the above definition you should also be able to state what is/are the
-- straight line segments with the most vertices, which is/are...?

-- Now, using the below, what is the longest (distance) straight-line segment?

straightLineSegmentLength :: FigureC -> String -> Maybe Float
straightLineSegmentLength fig line = undefined
