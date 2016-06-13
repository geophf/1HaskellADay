module Y2016.M06.D13.Exercise where

import Data.Matrix

{--
The problem with last week's exercises involving solving for internal 
intersectors of triangles (I'm talking the exercises for 2016-06-08 and 09),
is that these problems can be viewed algebraically as solving systems of 
equations, and a good way to represent a system of equations is with a matrix.

"So?" you prompt.

Well, the problem here is that the provled Data.Matrix module does not have
a system-of-equations-solver.

Today's exercise is ... guess!

Yes, you guessed: computing the inverse of a matrix, and, with the matrix
inverse solving a system of equations.

c.f.:

https://www.mathsisfun.com/algebra/systems-linear-equations-matrices.html

That is to say (spelling it out):

      x +  y +  z =  6
          2y + 5z = -4
     2x + 5y -  z = 27

are the AX = B matrices:

| 1 1  1 | | x |   |  6 |
| 0 2  5 | | y | = | -4 |
| 2 5 -1 | | z | = | 27 |

And X = (inv A) B

And therefore we have our solution to the three unknowns of the above system of
equations.

But it's not as simple as that, is it? The problem here is that to compute the
inverse you have to know the determinant of the matrix (well, its inverse), so
we have to compute the determinant to find the inverse.

So, today's Haskell exercise is REALLY: compute the determinant of a matrix:

ref: https://www.mathsisfun.com/algebra/matrix-determinant.html
--}

determinant :: Num a => Matrix a -> a
determinant = undefined

-- With the above defined, what are the determinants of the below matrices?

ex1, ex2, ex10 :: Matrix Float

ex1  = fromLists [[2,5], [1, (-3)]]
ex2  = fromLists [[3,(-5)],[2,1]]
ex10 = fromLists [[2,(-1),0], [3, (-5), 2], [1,4,(-2)]]

-- exercises are from mathopolis.com
