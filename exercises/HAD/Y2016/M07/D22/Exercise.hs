module Y2016.M07.D22.Exercise where

{--
So, yesterday's solution showed that we still had incomplete information, we
could path from 'a' to 'c' and 'b' to 't' (in a round-about way), but a direct
path in the figure 'b' to 'h' was shown the graph query could not find.

The problem with the pathing-algorithm is that it doesn't account for the
(directional) edge h -> k to be directional (so that the path could be found).

One solution, then, is to correct the pathing algorithm to treat the edge
h -> k as also the edge k -> h 

... so that's one approach.

Another approach is that during the ingest we can ensure all edges are
bidirectional when we are constructing the graph.

Either way. Choose a way that allows us to traverse the graph and answer,
correctly this time, pathing information in the graph.
--}

import Data.Graph

import Y2016.M07.D20.Solution (FigureC)
import Y2016.M07.D19.Exercise (figure2)
import qualified Y2016.M07.D19.Exercise as Incomplete

graphit :: Incomplete.Figure -> [(Char, Char)] -> FigureC
graphit = undefined

pathing :: FigureC -> Char -> Char -> String
pathing = undefined

-- Now with the new, correct, definitions, answer these pathing problems:

-- 1. what is the path from 'a' to 'c'?
-- 2. what is the path from 'b' to 't'?
-- 3. what is the path from 'b' to 'h'?
-- 4. what is the path from 'd' to 'f'?
-- 5. what is the path from 'm' to 'a'?
