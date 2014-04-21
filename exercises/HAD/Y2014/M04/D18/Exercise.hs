module HAD.Y2014.M04.D18.Exercise where

{- | combinationsOFDepth

   Given a length and a seed list, it provides all the combinations
   of elements of this list of this given legnth.

   Examples:

   >>> combinationOfDepth 2 "abc"
   ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]

   >>> combinationOfDepth 3 "ab"
   ["aaa","aab","aba","abb","baa","bab","bba","bbb"] 

   >>> combinationOfDepth 0 "abc"
   []

   >>> combinationOfDepth (-2) "abc"
   []
-}
combinationsOFDepth :: Int -> [a] -> [[a]]
combinationsOFDepth = undfined
