module Y2016.M07.D20.Solution where

import Control.Monad (join, (<=<))
import Data.Graph
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, fromJust)

import Control.Logic.Frege ((<<-))
import qualified Data.MultiMap as MM
import Y2016.M07.D19.Exercise (Point2d)
import qualified Y2016.M07.D19.Exercise as Incomplete

type Figure = Graph
type FigureC = (Figure, Vertex -> (Point2d, Char, String), Char -> Maybe Vertex)

lineSegments :: [(Char, Char)]  -- these are the edges missing from yesterday
lineSegments = zip "aaabbbggggcccddfffhhhhttjkmn"
                   "bjfjkgkmncnpdptjkhkmntnpkmnp"

-- first of all we want to convert the multimapping ('a','b'), ('a','j') to
-- the mapping ('a', "bj"), etc ... multimap will help here.

enqueue :: Ord a => [(a,a)] -> Map a [a]
enqueue = MM.store . MM.fromList pure

{--
*Y2016.M07.D20.Solution> enqueue lineSegments ~>
fromList [('a',"bjf"),('b',"jkg"),('c',"npd"),('d',"pt"),('f',"jkh"),
          ('g',"kmnc"),('h',"kmnt"),('j',"k"),('k',"m"),('m',"n"),
          ('n',"p"),('t',"np")]
--}

-- Okay, we need to put the old-figure map and the above map into GFF.
-- GFF: graph-friendly format

gff :: Map Char String -> Incomplete.Figure -> [(Point2d, Char, String)]
gff tree = map (\(key, pt) -> (pt, key, find key tree)) . Map.toList
   where find = catShift <<- Map.lookup

catShift :: Maybe [a] -> [a]
catShift = join . maybeToList

{--
*Y2016.M07.D20.Solution> gff (enqueue lineSegments) Incomplete.figure2 ~>
[((0.0,0.0),'a',"bjf"),((15.0,10.0),'b',"jkg"),((35.0,10.0),'c',"npd"),
 ((50.0,0.0),'d',"pt"),((15.0,-10.0),'f',"jkh"),((25.0,10.0),'g',"kmnc"),
 ((25.0,-10.0),'h',"kmnt"),((15.0,0.0),'j',"k"),((20.0,0.0),'k',"m"),
 ((25.0,0.0),'m',"n"),((30.0,0.0),'n',"p"),((35.0,0.0),'p',""),
 ((35.0,-10.0),'t',"np")]
--}

-- with this structure we can realize the graph from the vertices and edges

graphit :: Incomplete.Figure -> [(Char, Char)] -> FigureC
graphit fig2 = graphFromEdges . (`gff` fig2) . enqueue

{--
*Y2016.M07.D20.Solution> let (fig, lookupPtf, lookupVertM) = graphit Incomplete.figure2 lineSegments 
*Y2016.M07.D20.Solution> fig ~>
array (0,12) [(0,[1,7,4]),(1,[7,8,5]),(2,[10,11,3]),(3,[11,12]),(4,[7,8,6]),
              (5,[8,9,10,2]),(6,[8,9,10,12]),(7,[8]),(8,[9]),(9,[10]),(10,[11]),
              (11,[]),(12,[10,11])]
*Y2016.M07.D20.Solution> lookupPtf 0 ~> ((0.0,0.0),'a',"bjf")
--}

-- Given a vertex-as-character, return all the vertices it's directly connected
-- to, that is: its immediate neighbors. This is a trick(y) question as a
-- connection for b is both b -> g but also a -> b, so check thoroughly!

neighbors :: FigureC -> Char -> String
neighbors = catShift . fmap (\(_,_,s) -> s) <<- getC

getC :: FigureC -> Char -> Maybe (Point2d, Char, String)
getC (_, lkPtf, lkVm) = fmap lkPtf . lkVm

-- 1. What are the neighbors of 'a'? Should be "bjf" or a permutation of that.

-- *Y2016.M07.D20.Solution> neighbors figc 'a' ~> "bjf"

-- Did you encode your points fully?

loc :: FigureC -> Char -> (Char, Point2d)
loc = fromJust . fmap (\(pt, ch, _) -> (ch, pt)) <<- getC

-- 2. What is the location of pt a? Should be (0,0) What is the location of b?

{--
*Y2016.M07.D20.Solution> loc fig 'a' ~> ('a',(0.0,0.0))
*Y2016.M07.D20.Solution> loc fig 'b' ~> ('b',(15.0,10.0))
--}

-- connections: All the points in this figure are (somehow) connected. But how?

connection :: Figure -> Char -> Char -> String
connection fig from to = undefined

-- so breath-first-search on the forest rooted at 'a'? (forest-of-one-tree)?

-- maybe a problem for tomorrow? Yeah, sounds right.

-- There are many paths from, e.g. 'a' to 'c' ... pick one of them. Shortest
-- would be nice but not necessary. Should be "abgc"-ish

{--
And we have something like a definition to connection with:
*Y2016.M07.D20.Solution> let (g,pf,vf) = fig
*Y2016.M07.D20.Solution> vf 'a' ~> Just 0
*Y2016.M07.D20.Solution> dfs g [0] ~>
[N _0_ [N _1_ [N 7   [N 8   [N 9 [N 10 [N 11 []]]]],
               N _5_ [N _2_ [N 3 [N 12 []]]]],
        N 4   [N 6   []]]]

and we see that

*Y2016.M07.D20.Solution> pf 1 ~> ((15.0,10.0),'b',"jkg")
*Y2016.M07.D20.Solution> pf 5 ~> ((25.0,10.0),'g',"kmnc")
*Y2016.M07.D20.Solution> pf 2 ~> ((35.0,10.0),'c',"npd")

So we have a path (underscored) of "abgc" ... now extracting that path ... mm!
--}
