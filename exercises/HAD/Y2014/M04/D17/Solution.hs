module HAD.Y2014.M04.D17.Solution where

import Data.List (group,transpose)


{- | countEqualAdjacentPairs

   Count pairs of values that are equal in a matrix.

   Examples:

   >>> countEqualAdjacentPairs [[0,1],[0,2]]
   1
   >>> countEqualAdjacentPairs [[0,0],[1,2]]
   1
   >>> countEqualAdjacentPairs [[0,1],[0,0]]
   2
   >>> countEqualAdjacentPairs [['a','a'],['a','a']]
   4
   >>> countEqualAdjacentPairs [[0,0,0],[0,0,0],[0,0,0]]
   12
-}
countEqualAdjacentPairs :: Eq a => [[a]] -> Int
countEqualAdjacentPairs =
  length . (tail =<<) . (group =<<) . ((++) =<< transpose)
