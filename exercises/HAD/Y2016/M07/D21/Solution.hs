module Y2016.M07.D21.Solution where

import Data.Graph
import Data.Tree (subForest, rootLabel)
import Data.Maybe (maybeToList, mapMaybe)

import Y2016.M07.D20.Solution
import Y2016.M07.D19.Exercise (figure2)

{-- 
you can get a Graph-Figure of figure from:
*Y2016.M07.D21.Solution> let (gr,fnPt,fnVertM) = graphit figure2 lineSegments 
--}

pathing :: FigureC -> Char -> Char -> String
pathing fig@(gr,_,vf) start end = -- no path is "", so: 
   let dest      = head (maybeToList (vf end))
       branches  = subForest (head (dfs gr (maybeToList (vf start))))
       nextnodes = map rootLabel branches
   in  start : p' fig nextnodes dest

p' :: FigureC -> [Vertex] -> Vertex -> String
p' fig@(gr,toNodef,toVertf) roots dest =
   let branch = head (filter (flip (path gr) dest) roots)
       (_,label,branches) = toNodef branch
   in  label : if branch == dest then ""
               else p' fig (mapMaybe toVertf branches) dest

{--
What is _a_ pathing from 'a' to 'c'?

*Y2016.M07.D21.Solution> let fig = graphit figure2 lineSegments 
*Y2016.M07.D21.Solution> pathing fig 'a' 'c' ~> "abgc"

-- What is _a_ path from 'b' to 't'?

*Y2016.M07.D21.Solution> pathing fig 'b' 't' ~> "bgcdt"

Not the shortest path, but it is a path. However:

*Y2016.M07.D21.Solution> pathing fig 'b' 'h' ~>
"b*** Exception: Prelude.head: empty list

Shows us that the above does not treat all edges as bidirectional, as it says
the network does not support a b -> h path, when the figure shows there is such
a path. There are several ways to address this issue. We'll tackle this tomorrow.
--}
