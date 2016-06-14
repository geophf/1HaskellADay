module Y2016.M06.D14.Exercise where

import Data.Matrix

{--
Okay, so now that you have solved what the determinant of a matrix is 
(yesterday's exercise), today, let's solve what the inverse of a matrix is
and, with that, solve a system of equations.

There are multiple methods on how to invert a matrix. One approach is:

https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html

Whatever approach you choose it must be that:

(inverse a) `cross` a = identityMatrix 

and

a `cross` (inverse a) = identityMatrix

So, let's get to it:
--}

inverse :: Num a => Matrix a -> Matrix a
inverse = undefined

{-- 
With the above defined, you can solve systems of equations, for, as you recall

AX = B

represents

     A        X        B
--------------------------
| 2  1  2 | | x |   |  3 |
| 1 -1 -1 | | y | = |  0 |
| 1  1  3 | | z |   | 12 |

So:

X = (inverse A) `cross` B

So, given:
--}

a, b :: Matrix Float
a = fromLists [[2,1,2], [1, -1, -1], [1,1,3]]
b = transpose (fromLists [[3,0,12]]) -- note how b is defined

{--
Recall that you can 'view' matrices with pprint.

Now, set up the matrices and solve the below system of equations

  x + z  = 6
  z - 3y = 7
 2x + y + 3z = 15

with the below solver
--}

solver :: Matrix Float -> Matrix Float -> Matrix Float
solver = undefined
