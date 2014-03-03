module HAD.Y2014.M03.D03.Solution where

import GHC.Exts (sortWith)

-- | Sort a list of list of elements by the maximum of each list,
-- in ascending order
--
-- Point-free: easy and readable
-- Level: EASY
--
-- Examples:
-- >>> sortByMax [[1,10],[5,5]]
-- [[5,5],[1,10]]
-- >>> sortByMax []
-- []
-- 
-- sortByMax [[], [1,2]]
-- should throw an execption: no max for empty list

sortByMax :: Ord a => [[a]] -> [[a]]
sortByMax = sortWith maximum
