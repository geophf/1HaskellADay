module Y2017.M01.D13.Solution where

import Control.Arrow (first)
import Data.Array
import Data.Tuple (swap)

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
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
transposeAF = M . (array . adjoin swap . bounds <*> map (first swap) . assocs)
            . matrix

-- With your transposeAF definition verify tehMatrix is transposed.
-- What does transposeAF ourMatrix look like?

{--
We saw transposeAF tehMatrix above. It works.
*Y2017.M01.D13.Solution> pprint ourMatrix 
Matrix 5x5
|  1  2  3  4  5 |
|  6  7  8  9 10 |
| 11 12 13 14 15 |
| 16 17 18 19 20 |
| 21 22 23 24 25 |

So the transposition is:
*Y2017.M01.D13.Solution> pprint (transpose ourMatrix)
Matrix 5x5
|  1  6 11 16 21 |
|  2  7 12 17 22 |
|  3  8 13 18 23 |
|  4  9 14 19 24 |
|  5 10 15 20 25 |

Yup, that works.
--}
