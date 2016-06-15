module Y2016.M06.D15.Exercise where

import Data.Matrix

{--
Let's round out matrix for completeness sake, and the rest of the week we'll
look at charting data.

So, I, the identity matrix is:

|1|

or

| 1 0 |
| 0 1 |

or

| 1 0 0 |
| 0 1 0 |
| 0 0 1 |

or whatever size you need.

Its interesting properties are:

IA = AI = A

for some matrix A

define I:
--}

identity :: Num a => Int -> Matrix a
identity ncols = undefined

{--
Now show that the identity matrix is the identity matrix by crossing it with
ex1, for example. Generate the correctly sized identity matrix to cross with
any input matrix:
--}

sameMatrix :: Num a => Matrix a -> Matrix a
sameMatrix mat = undefined

-- mat `cross` identity (size?) == ident (size?) `cross` mat == mat
-- define sameMatrix along the above lines
