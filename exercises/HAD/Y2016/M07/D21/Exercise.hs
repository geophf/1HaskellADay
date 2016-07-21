module Y2016.M07.D21.Exercise where

{--
So, yesterday, we were able to translate the figure with its points and line
segments into a graph (à la Graph Theory) and make queries against that graph.

Great.

I left part of the exercise unfinished yesterday in that we have a graph
and, from that, we know paths of vertices, but vertices are not the points.

When I ask: "What is the path from points a to c?"

I'm expecting an answer: "abgc" 

What I am not expecting is a Forest of Vertex-values, but that's what I got,
and decoding those is a bear.

Today's Haskell exercise:

Decode a Forest of Vertex values back to the labels of the points of the
figure so that a path query makes sense to humble human creatures: the "wetware"
--}

import Data.Graph

import Y2016.M07.D20.Solution
import Y2016.M07.D19.Exercise (figure2)

{-- 
you can get a Graph-Figure of figure from:
*Y2016.M07.D21.Exercise> let (gr,fnPt,fnVertM) = graphit figure2 lineSegments 
--}

pathing :: Figure -> Char -> Char -> String
pathing fig start end = undefined

-- What is _a_ pathing from 'a' to 'c'?

-- Hint: Data.Graph.dfs gr [0] is the Forest of Vertex-values from 0, and
-- we know 0 is the Vertex for 'a', because fnPt 0 ~> ((0.0,0.0),'a',"bjf")

-- What is _a_ path from 'b' to 't'?
