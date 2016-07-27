module Y2016.M07.D27.Solution where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.List (group, sort)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

import Y2016.M07.D19.Solution (figure2)
import Y2016.M07.D20.Solution (FigureC, lineSegments)
import Y2016.M07.D22.Solution (graphit)
import Y2016.M07.D26.Solution (extendPath)

-- Now, for the distance function, the question arises: as the crow flies? or
-- only along a path? The latter is harder because it already solves 
-- shortestDistancePathing problem. So let's just go with 'as the crow flies.'

distance :: FigureC -> Char -> Char -> Maybe Float
distance fig@(gr, ptInfoF, vertF) start end =
   vertF start >>= \v0 -> let ((x1, y1), _, _) = ptInfoF v0 in
   vertF end   >>= \v1 -> let ((x2, y2), _, _) = ptInfoF v1 in
   return (sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2))

{--
*Y2016.M07.D27.Solution> distance fig 'a' 'b'
Just 18.027756
*Y2016.M07.D27.Solution> distance fig 'a' 'j'
Just 15.0

Yay, and yay!
--}

-- Okay, shortest distance. One way to solve this problem is by brute force,
-- another way is by iterative deepening on the distance (maintaining the 
-- in-the-running distances as we deepen) and others are from various search
-- survey courses ACO, genetics, etc.

-- let's start with Brute Force (caps), because I always start with that, then
-- always regret starting with that, then improve if necessary:
-- this is the agile approach:
-- 1. Make it work
-- 2. make it right
-- 3. make it fast
-- Let's make it work, first

allPaths :: FigureC -> Char -> Char -> [String]
allPaths fig start = 

-- now 'allPaths' is simply the work I did yesterday that accidently discovered
-- all the paths by not stopping at the shortest ones whilst iteratively
-- deepening:

   flip (allps fig) (extendPath fig (Set.empty, pure start))

allps :: FigureC -> Char -> [(Set Char, String)] -> [String]
allps fig end vps = case filter ((== end) . head . snd) vps of
   []   -> concatMap (allps fig end . extendPath fig) vps
   anss -> map (reverse . snd) anss

{--
*Y2016.M07.D27.Solution> let fig = graphit figure2 lineSegments 
*Y2016.M07.D27.Solution> let ab = allPaths fig 'a' 'b' ~> ["ab"]
*Y2016.M07.D27.Solution> let ac = allPaths fig 'a' 'c'
*Y2016.M07.D27.Solution> length ac ~> 191
*Y2016.M07.D27.Solution> take 5 ac ~>
["abjfkgc","abjfkhmgc","abjfkhmnc","abjfkhnc","abjfkhtdc"]
--}
 
-- brute force method: find all distances of all paths, sort by distance,
-- then group-by

shortestDistancePathing :: FigureC -> Char -> Char -> [String]
shortestDistancePathing fig start =
   map snd . head . group . sort . map (distances fig &&& id)
           . allPaths fig start

distances :: FigureC -> String -> Maybe Float
distances fig [a,b] = distance fig a b
distances fig (a:b:rest) =
   liftM2 (+) (distance fig a b) (distances fig (b:rest))

-- So, with shortestDistancePathing defined, find the shortest distance between
-- nodf 'm' and 'a' in the figure from the value of:

{--
*Y2016.M07.D27.Solution> shortestDistancePathing fig 'a' 'c' ~> ["abgc"]
*Y2016.M07.D27.Solution> shortestDistancePathing fig 'm' 'a' ~> ["mkja"]

YAY!

We're a LITTLE bit closer to determining what trigons are (what we lead off 
with two weeks ago), now we must determine what points are in a straight line,
shortestDistancePathing helps us.

We'll look at finding straight lines tomorrow.
--}
