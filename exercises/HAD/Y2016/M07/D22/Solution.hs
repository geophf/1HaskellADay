module Y2016.M07.D22.Solution where

{--
We're going to go with making sure the graph is built correctly at ingest, 
because mucking with algorithms always leads to teh Sadeness.
--}

import Control.Arrow ((&&&))
import Data.Graph

import Y2016.M07.D20.Solution (FigureC, lineSegments)
import Y2016.M07.D19.Exercise (figure2)
import qualified Y2016.M07.D19.Exercise as Incomplete
import qualified Y2016.M07.D20.Solution as Almost
import qualified Y2016.M07.D21.Solution as Complete

graphit :: Incomplete.Figure -> [(Char, Char)] -> FigureC
graphit fig = Almost.graphit fig . uncurry (++) . (map swap &&& id) 

-- swap ensures graph that has edge (b,a) also has edge (a,b)

swap :: (a,b) -> (b,a)
swap = snd &&& fst

pathing :: FigureC -> Char -> Char -> String
pathing = Complete.pathing

-- so we just path with the graph with bidirectional edges, right?

-- As in yesterday's exercise:

-- let fig@(gr,fnPt,fnVertM) = graphit figure2 lineSegments 
-- gets you the graph and

-- Now with the new, correct, definitions, answer these pathing problems:

-- 1. what is the path from 'a' to 'c'?

-- *Y2016.M07.D22.Solution> pathing fig 'a' 'c' ~> "ababababababa...

-- nope: doesn't handle cycles

-- Next week's exercise will look at handling cycles in graphs for pathing.
