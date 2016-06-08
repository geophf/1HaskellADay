module Y2016.M06.D08.Exercise where

import Data.Graphics.SVG         -- http://lpaste.net/309926248828633088

{--
@1HaskellADay problem for 2016-06-08

ref: maths/triangle.hs

Time to get a little math-y homework-y for, oh, no particular reason.

Let's pretend you're helping your teen with the math homework, and let's
pretend you need to, you know, check your work ... I mean your teen's work.

Given:

1. a line at the origin with a positive slope

        y1 = m1 * x1

2. a line that crosses the X-axis some (positive) distance from the origin
   with a negative slope.

        y2 = m2 * x2 + b

(so m2 is negative)

At what point on the (x,y) plane do these lines intersect?

Put another way: 

1. how do you model two-dimentional linear functions? 
2. how do you solve this system of two equations and two unknowns?
--}

data Line a = YisMXplusB   -- You declare this to work with your system.
type Point a = (a,a)

solver :: Num a => Line a -> Line a -> Point a
solver = undefined

-- what are the results for the following values of m1, m2, and b?

msandbs :: [(Rational, Rational, Rational)]
msandbs = [(2, (-0.5), 4), (3, 2.3, 7)]

{-- BONUS ----------------------------------------------------------------

fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffhjhhhjjjv csxllll

^-- eheh, this isn't me cursing, nor some new weird encryption-scheme, nope:
it's me falling asleep at the keyboard whilst composing today's problem!

WARNING: composing math problems can be detrimental to ASCII character use!

Okay, anyway.

Today's bonus is this: using whatever graph representation you choose, 
including your spreadsheet software, if you'd like, draw a couple of examples 
of the above problem.

HINT: you can draw it in an SVG bounding box, too (imported above)
--}

lines :: Num a => [Line a] -> IO ()
lines = undefined
