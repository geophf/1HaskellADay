module Y2017.M07.D06.Exercise where

-- below import available via 1HaskellADay git repository

import Data.Matrix

{--
Okay, I've asked this WAY back in ... 2014? 2015? but I can't find the solution,
so I'll ask it again.

Write a matrix system-of-equations solver.

This is easier done than said? ... maybe?

First, let's say we have the system of equations:

 2x - y = 1
  x - y = -1

Simple enough to solve by hand, sure! So we can test it out on our solver, too!

To solve a system of equations, we take the inverse of the matrix of 
coefficients, A, and multiply it by the matrix of constants, B to get our
solution-set, or:

A^-1 B

So, we've got to figure out how to invert a matrix. That's simple enough, given
that we've previously defined the matrix-determinant (see Data.Matrix).

OR you could use the Gauss-Jordan-approach.

Whatever tickles your fancy.
--}

inverse :: Fractional a => Matrix a -> Maybe (Matrix a)
inverse mat = undefined

-- once you've defined inverse, use it to solve the above system

a, b :: Matrix Float
a = fromLists [[2, -1], [1, -1]]
b = fromLists [[1], [-1]]

-- what is inverse a `cross` b?
