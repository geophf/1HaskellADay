module Y2016.M06.D09.Exercise where

import Data.Graphics.SVG         -- http://lpaste.net/309926248828633088

{--
@1HaskellADay problem for 2016-06-09

ref: maths/bisector.hs

Okay, you know what a triangle looks like. Good.

Today we'll make (at least) two of them

Like yesterday, realize a triangle from the below data sets, but this time,
from one of the vertices, extend a line to the mid-point of the opposed side.

OOH! HOW MANY TRIANGLES DOES THAT MAKE?

Okay, so great, but what is the equation of that line?
--}

data Line a = YisMXplusB   -- You declare this to work with your system.
type Point a = (a,a)

bisector :: Num a => Line a -> Line a -> Line a
bisector = undefined

-- what are the results for the following values of m1, m2, and b?

msandbs :: [(Rational, Rational, Rational)]
msandbs = [(2, (-0.5), 4), (3, 2.3, 7)]

{-- BONUS -----------------------------------------------------------------

Like yesterday, draw the triangle, ... triangleSSSSS as you've got more now
with the bisecting line.
--}

lines :: Num a => [Line a] -> IO ()
lines = undefined -- hint: perhaps definition won't change?

{-- BONUS-BONUS -----------------------------------------------------------

Bisect from all three vertices. What is the point at which the triple
bisection meets?

All you are given is two sides (lines, actually) of the triangle, the third
side is the X axis, from there you must define the rest of the solution.
--}

tripleThreat :: Num a => Line a -> Line a -> Point a
tripleThreat = undefined
