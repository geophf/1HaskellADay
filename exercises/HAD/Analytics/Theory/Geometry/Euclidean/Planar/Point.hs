module Analytics.Theory.Geometry.Euclidean.Planar.Point where

import Data.Graph
import Data.Array

-- We look at points and lines here.

data Point a = P { x :: a, y :: a }
   deriving (Eq, Ord, Show)

-- we provide a Sierpinski triangle as demonstration

edgesSier :: Graph
edgesSier =
   buildG (1,15) [(1,2),(1,4),(1,9),(1,3),(1,7),(1,15),
                  (2,3),(2,5),(2,4),
                  (3,7),(3,5),
                  (4,13),(4,6),(4,9), (4,8),(4,5),(4,7),
                  (5,7),
                  (6,8),(6,11),(6,9),
                  (7,12),(7,15),(7,10),(7,13),
                  (8,13),(8,11),
                  (9,11),(9,13),(9,15),
                  (10,12),(10,14),(10,13),
                  (11,13),
                  (12,14),(12,15),
                  (13,14),(13,15),
                  (14,15)]

vertexL :: [Point Rational]

--                 1      2      3      4       5       6       7      8   
vertexL = map (uncurry P)
                [(0,0), (3,3), (7,0), (7,7), (11,3), (11,11),(15,0),(15,7),
--                  9     10/A   11/B    12/C   13/D   14/E   15/F
                 (15,15),(19,3),(19,11),(23,0),(23,7),(27,3),(30,0)]

verticesSier :: [Point a] -> Array Int (Point a)
verticesSier verts = array (1,length verts) $ zip [1..] verts

allInTheSameLine :: (Fractional a, Num a, Eq a) => Vertex -> Vertex -> 
                                                   Vertex -> [Point a] -> Bool
allInTheSameLine a b c verts =
   let [p1, p2, p3] = map (verticesSier verts !) [a,b,c]
       slope1       = slope p1 p2
       b1           = offset slope1 p1
   in  yEqmxPlusb p1 slope1 b1 -- so, this HAS TO BE true, right?
    && yEqmxPlusb p2 slope1 b1 -- so is this
    && yEqmxPlusb p3 slope1 b1 -- and here's the test

slope :: (Fractional a, Num a) => Point a -> Point a -> a
slope (P x1 y1) (P x2 y2) = (y2 - y1) / (x2 - x1)

offset :: (Fractional a, Num a) => a -> Point a -> a
offset slop (P x y) = slop * x - y

yEqmxPlusb :: (Num a, Eq a) => Point a -> a -> a -> Bool
yEqmxPlusb (P x y) m b = y == m * x + b

{--

So:

*Main> allInTheSameLine 1 2 4 ~> True
*Main> allInTheSameLine 1 3 7 ~> True
*Main> allInTheSameLine 1 2 9 ~> True
*Main> allInTheSameLine 1 4 7 ~> False

Okay, that worked; Yay! Of course, this presupposes verticesSier, so
to be completely independent of that, we would need to pass in an
array of vertex-locations, but, yeah; you get it.

p.s.: okay, abstracted the vertex-list, so now we can further test
allInTheSameLine:

*Main> allInTheSameLine 1 2 3 vertexL ~> False -- Good!
*Main> allInTheSameLine 1 2 4 vertexL ~> True  -- okay, we're still working

Now, let's add a couple of vertices:

--}

extendedLine :: [Point Rational]
extendedLine = vertexL ++ [P 27 0]

{-- so, the above tests work still, and:

*Main> allInTheSameLine 1 2 16 vertexL ~>
*** Exception: Ix{Int}.index: Index (16) out of range ((1,15))

Of course, ... but then:

*Main> allInTheSameLine 1 2 16 extendedLine ~> False -- right
*Main> allInTheSameLine 1 3 16 extendedLine ~> True  -- sweet!

Okay, now, let's have a point out in spaaaaacccceee! --}

leftField :: [Point Rational]
leftField = extendedLine ++ [P 28 13]

{-- and so:

*Main> allInTheSameLine 1 3 16 leftField ~> True  -- sweet!
*Main> allInTheSameLine 1 3 17 leftField ~> False -- Sweetnessatude!

Okay, and I'm outie! --}
