module HAD.Y2014.M03.D04.Exercise where

-- | checkSort verify if a given list is sorted
-- Point-free version is hard to find BUT is readable.
-- Level: Medium
--
-- Examples:
--
-- >>> checkSort ([1..10] :: [Int])
-- True
--
-- >>> checkSort $ ([1,3,2] :: [Int])
-- False
--
-- >>> checkSort []
-- True
--
checkSort :: Ord a => [a] -> Bool
checkSort =  undefined
