module Y2017.M01.D13.Exercise where

import Data.Array

-- below import available via 1HaskellADay git repository

import Data.Matrix

{--
We're going to start looking at Applicative functors, because Applicative
functors are more cool than (Haskell) Monads, and almost as cool as Arrows.

And because there's a bunch of exercises around Applicative functors that
lead to Traversable, which are supposed to be hella cool, AND which have
a set of exercises around them, so next week we'll look at both Applicative
functors and Traversable types.

So let's start with Applicative functors.

Connor McBride in his "Applicative Programming with Effects" gives one case
for Applicative functors in that it trivializes Matrix transposition.

So, okay: challenge accepted.

The transposition of the matrix tehMatrix:

*Data.Matrix> pprint tehMatrix
Matrix 2x3
| 1 2 3 |
| 4 5 6 |

is:

*Data.Matrix> pprint (transpose tehMatrix)
Matrix 3x2
| 1 4 |
| 2 5 |
| 3 6 |

The thing is: I defined transpose using arrows, not using applicative functors.

Connor McBride says transpose can be defined trivially with applicative functors

So:
--}

transposeAF :: Matrix a -> Matrix a
transposeAF = undefined

-- where transposeAF transposes a matrix using Applicative Functors.

-- hint: look at the definition of transpose; transposeAF should be a trivial
-- reduction.

-- With your transposeAF definition verify tehMatrix is transposed.
-- What does transposeAF ourMatrix look like?
