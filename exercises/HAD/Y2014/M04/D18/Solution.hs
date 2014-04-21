module HAD.Y2014.M04.D18.Solution where

import Control.Applicative (liftA2)

{- | combinationsOfDepth

   Given a length and a seed list, it provides all the combinations
   of elements of this list of this given legnth.

   Examples:

   >>> combinationsOfDepth 2 "abc"
   ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]

   >>> combinationsOfDepth 3 "ab"
   ["aaa","aab","aba","abb","baa","bab","bba","bbb"] 

   >>> combinationsOfDepth 0 [1,2,3]
   [[]]

   >>> combinationsOfDepth (-2) "abc"
   [""]
-}
combinationsOfDepth :: Int -> [a] -> [[a]]
combinationsOfDepth = replicateM
     
