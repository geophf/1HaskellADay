module Y2016.M09.D15.Exercise where

-- the below import is available from the 1HaskellADay git repository

import Data.Matrix

{--
Today's Haskell exercise is neither sudoku nor magic squares. We're just working
with one square, only, and arranging the number 1 through 9 in those squares.

Simple enough.

Oh, there's a few constraints.

So, one way to arrange the numbers 1 through 9 in a 3x3 matrix is as follows:
--}

threeBy :: Matrix Int
threeBy = fromLists (take 3 (counting [1,2,3]))

counting :: [Int] -> [[Int]]
counting = (map . (+) . length) >>= iterate  -- via joomy @cattheory

{--
*Y2016.M09.D15.Exercise> pprint threeBy
Matrix 3x3
| 1 2 3 |
| 4 5 6 |
| 7 8 9 |
--}

-- But today's problem is a mite harder that threeBy. Here are the constraints:
-- 1. 1 is two squares directly right of 7
-- 2. 2 is two squares directly above 8
-- 3. 3 is two squares directly left of 9
-- 4. 4 is two squares directly below 3
-- 5. 5 is not in the center square

-- create a schema for the constraints so that the constraints hold and the
-- numbers 1 - 9 are arranged in a 3x3 matrix.

data Constraint = YouDeclareYourConstraintSchema

constrainedMatrix :: [Constraint] -> [Int] -> [Matrix Int]
constrainedMatrix constraints nums = undefined

-- so constraintedMatrix guards [1..9] gives a properly arrayed matrix
