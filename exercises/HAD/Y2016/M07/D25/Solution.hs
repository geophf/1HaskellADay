module Y2016.M07.D25.Solution where

{--
We're going to go with making sure the graph is built correctly at ingest, 
because mucking with algorithms always leads to teh Sadeness.
--}

import Control.Arrow ((&&&))
import Data.Graph
import Data.Tree (subForest, rootLabel)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Y2016.M07.D20.Solution (FigureC, lineSegments)
import Y2016.M07.D19.Exercise (figure2)
import qualified Y2016.M07.D19.Exercise as Incomplete
import qualified Y2016.M07.D20.Solution as Almost
import qualified Y2016.M07.D21.Solution as Complete
import Y2016.M07.D22.Solution (graphit)

cyclicPathing :: FigureC -> Char -> Char -> String
cyclicPathing fig@(gr,_,vf) start end = -- "" represents no path, so: 
   let dest      = vert vf end
       src       = vert vf start
       tree      = head (dfs gr [src])
       branches  = subForest tree
       nextnodes = map rootLabel branches
   in  start : cp' fig nextnodes dest (Set.singleton start)

vert :: (Char -> Maybe Vertex) -> Char -> Vertex
vert vf = fromJust . vf

cp' :: FigureC -> [Vertex] -> Vertex -> Set Char -> String
cp' fig@(gr,toNodef,toVertf) roots dest visited =
   let allpaths         = filter (flip (path gr) dest) roots
       labeledPaths     = map ((\(_,lbl,br) -> (lbl,br)) . toNodef) allpaths
       directedpaths    = filter ((`Set.notMember` visited) . fst) labeledPaths

-- we need a sort by shortest distance to dest function here!

       (label,branches) = head directedpaths
       saw              = vert toVertf label
   in  label : if saw == dest then ""
               else cp' fig (mapMaybe toVertf branches) dest
                        (Set.insert label visited)

{-- Now with the new, correct, definitions, answer these pathing problems:

*Y2016.M07.D25.Solution> let fig = graphit figure2 lineSegments 

1. what is the path from 'a' to 'c'?
*Y2016.M07.D25.Solution> cyclicPathing fig 'a' 'c' ~> "abjfkgmhnc"

Hm (much) longer than anticipated.

2. what is the path from 'b' to 't'?
*Y2016.M07.D25.Solution> cyclicPathing fig 'b' 't' ~> "bajfkgmhncpdt"

3. what is the path from 'b' to 'h'?
*Y2016.M07.D25.Solution> cyclicPathing fig 'b' 'h' ~> "bajfkgmh"

But we are getting a path from b to h, which we did not get before with the
(incomplete) acyclic graph.

4. what is the path from 'd' to 'f'?
*Y2016.M07.D25.Solution> cyclicPathing fig 'd' 'f' ~> "dcgbajf"

5. what is the path from 'm' to 'a'?
*Y2016.M07.D25.Solution> cyclicPathing fig 'm' 'a' ~> "mgba"

This ties for shortest path, but this raises another problem: it's not the
straightest path.

We'll look at shortest path tomorrow, and we now need to look at something 
called 'straightest' path or 'linear' path. Okay.
--}
