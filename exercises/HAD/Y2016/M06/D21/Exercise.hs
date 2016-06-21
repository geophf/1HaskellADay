module Y2016.M06.D21.Exercise where

{--
So what happens when things become Complex?

With the Quadratic type, enhance solver in such a way that it gives all
solutions, whether they be Real or Complex.

This exercise is an elaboration of yesterday's that solved quadratic equations
with Real roots with the quadratic formula:

    -b +/- sqrt (b^2 - 4ac)
x = -----------------------
              2a

for the quadratic equation

ax^2 + bx + c = 0
--}

import Data.Complex

data QuadraticEq = Q { a,b,c :: Float }

solver :: QuadraticEq -> [Complex Float]
solver = undefined

-- with the new solver, solve the below quadratic equations represented by
-- the coefficients (a,b,c):

eqs :: [(Float, Float, Float)]
eqs = [(1,-6,25), (1,10,29), (1,-6,13), (2,6,29)]

{-- BONUS ------------------------------------------------------------------

The default Show-instance for QuadraticEq 'leaves something to be desired.'

Write your own Show-instance for QuadraticEq that for (Q 2 3 4) shows:

"2x^2 + 3x + 4 = 0"
--}

instance Show QuadraticEq where show = undefined
