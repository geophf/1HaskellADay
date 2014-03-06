module HAD.Y2014.M03.D06.Solution where

import Control.Applicative

-- | takeStrictlyLessThan take elements of a list while their sum is
-- _strictly_ less than a given number
--
-- Point-free: I didnt' try without parameter, you can easily "hide" the 2nd
-- parameter (ie. takeStrictlyLessThan x = …)
-- Level: MEDIUM
--
-- Examples:
-- >>> takeStrictlyLessThan (10::Int) [1..]
-- [1,2,3]
-- 
-- >>> takeStrictlyLessThan (3::Integer) $ repeat 1
-- [1,1]
--
-- >>> takeStrictlyLessThan (42::Int) $ []
-- []
--
takeStrictlyLessThan :: (Num a, Ord a) => a -> [a] -> [a]
takeStrictlyLessThan x = 
  map fst . takeWhile ((x>) . snd) . (zip <*> scanl1 (+))
