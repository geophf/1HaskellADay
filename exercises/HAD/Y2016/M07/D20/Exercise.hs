module Y2016.M07.D20.Exercise where

{--
After posting yesterday's problem I realized that Figure was missing a vital
set of information. Do you know what that missing information was? It had the
points of the figure, but which points are connected to which points?

That's the missing information, and without that piece, it's mighty hard to
verify what are trigons and what are not in to the figure.

So, let's rectify that. But how? Well, obviously, by adding in the lines by 
saying, e.g.: ('a', 'b') denotes the line segment connecting point 'a' to 
point 'b'.

In Graph Theory we call ('a', 'b') an edge and we call 'a' and 'b' vertices.

So, why not just put all this information of the Figure into a Graph?

Why not, indeed?

A (slight) problem is that Data.Graph.Vertex is an Int, and we must also retain
that 'a' is at Point2D (0,0) ... the Graph loses all that information, just
encoding the Vertex as Int and nothing else ... OR DOES IT? 

Today's Haskell exercise is to represent the Figure as a Graph and to answer
the associated questions.
--}

import Data.Graph
import Y2016.M07.D19.Exercise (Point2d)
import qualified Y2016.M07.D19.Exercise as Incomplete

type Figure = Graph

lineSegments :: [(Char, Char)]  -- these are the edges missing from yesterday
lineSegments = zip "aaabbbggggcccddfffhhhhttjkmn"
                   "bjfjkgkmncnpdptjkhkmntnpkmnp"

graphit :: Incomplete.Figure -> [(Char, Char)] -> Figure
graphit figure2 lineSegments = undefined

{--
Okay, now that we have a graph-representation of our figure, we can find out
interesting bits of graphy-information from the graph. Let's.
--}

-- Given a vertex-as-character, return all the vertices it's directly connected
-- to, that is: its immediate neighbors. This is a trick(y) question as a
-- connection for b is both b -> g but also a -> b, so check thoroughly!

neighbors :: Figure -> Char -> String
neighbors fig pt = undefined

-- 1. What are the neighbors of 'a'? Should be "bjf" or a permutation of that.

-- Did you encode your points fully?

loc :: Figure -> Char -> (Char, Point2d)
loc fig pt = undefined

-- 2. What is the location of pt 'a'? Should be (0,0) What is the location of 'b'?

-- connections: All the points in this figure are (somehow) connected. But how?

connection :: Figure -> Char -> Char -> String
connection fig from to = undefined

-- There are many paths from, e.g. 'a' to 'c' ... pick one of them. Shortest
-- would be nice but not necessary. Should be "abgc"-ish

-- Okay, have fun!
