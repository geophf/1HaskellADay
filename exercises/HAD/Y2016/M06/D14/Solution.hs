module Y2016.M06.D14.Solution where

import Control.Arrow ((&&&))
import Data.Array (listArray, elems)

import Data.Matrix

{-- Inverting a matrix ... because we can!

We'll use the minors/cofactors/adjugate(transpose) method described here:

https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html

Whatever approach you choose it must be that:

(inverse a) `cross` a = identityMatrix 

and

a `cross` (inverse a) = identityMatrix

So, let's get to it:
--}

minors :: Matrix a -> [[Matrix a]]
minors = uncurry map . (excludedSubMatrices &&& enumFromTo 1 . fst . snd . dims)

{-- So the minor of a is: fromLists (map (map determinant) (minors a))
*Y2016.M06.D14.Solution> pprint (fromLists (map (map determinant) (minors a)))
Matrix 3x3
| -2.0  4.0  2.0 |
|  1.0  4.0  1.0 |
|  1.0 -4.0 -3.0 |

Now we cofactors the minor:
--}

cofactors :: Num a => Matrix a -> Matrix a
cofactors mat = 
   M . listArray (dims mat)
     . zipWith (*) (cycle [1,-1])
     . elems $ matrix mat

{--
*Y2016.M06.D14.Solution> pprint (cofactors (fromLists (map (map determinant) (minors a))))
Matrix 3x3
| -2.0 -4.0  2.0 |
| -1.0  4.0 -1.0 |
|  1.0  4.0 -3.0 |

And now we just transpose that and multiply it by 1/det of the original matrix
to compute the inverse matrix:
--}

scalar :: Num a => a -> Matrix a -> Matrix a
scalar n mat = M . listArray (dims mat) . map (* n) . elems $ matrix mat

inverse :: RealFrac a => Matrix a -> Matrix a
inverse mat =
   let det = determinant mat
       comat = cofactors (fromLists (map (map determinant) (minors mat)))
   in  transpose ((1.0 / det) `scalar` comat)

{-- 
*Y2016.M06.D14.Solution> pprint (inverse a)
Matrix 3x3
|   0.5  0.25 -0.25 |
|   1.0  -1.0  -1.0 |
|  -0.5  0.25  0.75 |

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
solver a b = inverse a `cross` b

{--
*Y2016.M06.D14.Solution> pprint (solver a b)
Matrix 3x1
| -1.5 |
| -9.0 |
|  7.5 |

let's set up the second batch:
--}

c, d :: Matrix Float
c = fromLists [[1,0,1],[0,-3,1],[2,1,3]]
d = transpose $ fromLists [[6,7,15]]

{--
*Y2016.M06.D14.Solution> pprint (solver c d)
Matrix 3x1
|  2.0 |
| -1.0 |
|  4.0 |

which solves the systems of equations. YAY!

solver and inverse and its functions will be rolled into Data.Matrix
--}
