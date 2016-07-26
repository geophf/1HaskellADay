{-# LANGUAGE TupleSections #-}

module Y2016.M07.D26.Solution where

{--
So, we need to take a different approach here. The cyclicalPathing guarantees
returning a path from one node to another, if there is one, but we need, now
to find the least nodes visited path.

How do we do that? Iterative deepening suggests itself, and we stop the deepening
at the level that arrives at the destination node first.
--}

import Data.Graph (edges)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Y2016.M07.D19.Solution (figure2)
import Y2016.M07.D20.Solution (FigureC, lineSegments)
import Y2016.M07.D22.Solution (graphit)

leastVerticesPathing :: FigureC -> Char -> Char -> [String]
leastVerticesPathing fig start =
   flip (explorePaths fig) (extendPath fig (Set.empty, pure start))

{-- 
A bit of exploration that went down the wrong path: this eventually returned
all paths between two nodes (cycle-free)

lvp :: FigureC -> Char -> [(Set Char, String)] -> [String]
lvp fig end visitedpaths =
-- tried continuations:
   (`runCont` id) $ do
      ans <- callCC $ \foundIt -> do
         case filter ((== end) . head . snd) visitedpaths of
            []   -> return (explorePaths fig end visitedpaths)
            anss -> foundIt (map (reverse . snd) anss)
      return ans
--  nope.
   case filter ((== end) . head . snd) visitedpaths of
      []   -> explorePaths fig end visitedpaths
      anss -> map (reverse . snd) anss
--}

-- explorePaths is simply the lvp extended over the next layer of visited paths

explorePaths :: FigureC -> Char -> [(Set Char, String)] -> [String]
-- explorePaths fig end = concatMap (lvp fig end . extendPath fig)
explorePaths fig end vp = 
   let newpaths = concatMap (extendPath fig) vp
       anss     = filter ((== end) . head . snd) newpaths
   in  if null anss then explorePaths fig end newpaths 
       else map (reverse . snd) anss

-- and here we extend a path to the set of its possible futures
-- ... pruning cycles as we go.

extendPath :: FigureC -> (Set Char, String) -> [(Set Char, String)]
extendPath fig (no, path@(p:_)) =
   let possibles = neighbors fig p   -- first we get the extensions
       pruned = filter (`Set.notMember` no) possibles  -- next: prune
       visited = Set.insert p no
   in  map ((visited,) . (: path)) pruned   -- to get our possible futures

{--
The question is: how does one find all the neighboring nodes of a node? Easily!
You simply find all the edges that start with node n, and all the connected nodes
of those edges are the neighboring nodes.

Now, unfortunately, there's a translation from vertex id (an Int) to node label
(a Char), but, so: okay.
--}

neighbors :: FigureC -> Char -> String
neighbors fig@(gr, ptInfoF, vertF) start =
   maybeToList (vertF start)                          >>= \v0 ->
   map ((\(a,b,c) -> b) . ptInfoF . snd) (filter ((== v0) . fst) (edges gr))

-- *Y2016.M07.D26.Solution> let fig = graphit figure2 lineSegments 
-- *Y2016.M07.D26.Solution> neighbors fig 'a' ~> "bjf"

-- sure, obviously. How about something a little more challenging?

-- *Y2016.M07.D26.Solution> filter ((== 'k') . fst) lineSegments ~> [('k','m')]

-- that says 'k' has only one out-edge, going to 'm', but the figure shows much
-- more. Let's check: *Y2016.M07.D26.Solution> neighbors fig 'k' ~> "bgfhjm"
-- WOOT! Complete information.

{--
Note the care in the name of this function. Least nodes visited is not 
necessarily the shortest path, looking at the figure (for example, yesterday's
'm' to 'a' path was "mgba" which is, distance-wise, not the shortest, even though
four nodes is the (tied) least number of nodes. We will look at adding distance
information for shortest distance path tomorrow.
--}

{--
Okay, let's test'r out!

*Y2016.M07.D26.Solution> let fig = graphit figure2 lineSegments 
*Y2016.M07.D26.Solution> leastVerticesPathing fig 'a' 'c' ~> ["abgc"] -- WOOT!
*Y2016.M07.D26.Solution> leastVerticesPathing fig 'b' 'h' ~> ["bkh"]  -- WOOT!
*Y2016.M07.D26.Solution> leastVerticesPathing fig 'm' 'a' ~>
["mgba","mhfa","mkba","mkfa","mkja"]

... but which one of these is shortest?

We'll look at using the node information (which includes xy-location) tomorrow
--}
