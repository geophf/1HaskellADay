module Y2016.M06.D21.Solution where

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

q :: (Float, Float, Float) -> QuadraticEq
q (a,b,c) = Q a b c

solver :: QuadraticEq -> [Complex Float]
solver q = [φ] <*> [(+),(-)] <*> [q]

φ :: (Float -> Float -> Float) -> QuadraticEq -> Complex Float
φ f q = let imag2 = b q ^ 2 - 4 * a q * c q
            den   = 2 * a q
            real  = negate (b q) / den
            imag  = sqrt (abs imag2) / den in
   if imag2 < 0 then real :+ f 0 imag else f real imag :+ 0

-- with the new solver, solve the below quadratic equations represented by
-- the coefficients (a,b,c):

eqs :: [(Float, Float, Float)]
eqs = [(1,-6,25), (1,10,29), (1,-6,13), (2,6,29)]

{--
*Y2016.M06.D21.Solution> map (solver . q) eqs ~>
[[3.0 :+ 4.0,3.0 :+ (-4.0)],[(-5.0) :+ 2.0,(-5.0) :+ (-2.0)],
 [3.0 :+ 2.0,3.0 :+ (-2.0)],[(-1.5) :+ 3.5,(-1.5) :+ (-3.5)]]
--}

{-- BONUS ------------------------------------------------------------------

The default Show-instance for QuadraticEq 'leaves something to be desired.'

Write your own Show-instance for QuadraticEq that for (Q 2 3 4) shows:

"2x^2 + 3x + 4 = 0"
--}

instance Show QuadraticEq where
   show (Q a b c) =
      mbshow a ++ "x^2" ++ showSign b ++ "x" ++ showSign c ++ " = 0"

showSign :: Float -> String
showSign x = ' ':(if x < 0 then '-' else '+'):' ':mbshow (abs x)

mbshow :: Float -> String
mbshow x = if x == 1 then "" else showdec x

showdec :: Float -> String
showdec x = if floor x == ceiling x then show (floor x) else show x

{--
*Y2016.M06.D21.Solution> map q eqs ~>
[x^2 - 6x + 25 = 0,x^2 + 10x + 29 = 0,x^2 - 6x + 13 = 0,2x^2 + 6x + 29 = 0]
--}
