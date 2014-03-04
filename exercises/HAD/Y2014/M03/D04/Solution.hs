module HAD.Y2014.M03.D04.Solution where

import Control.Arrow ((&&&))
import Control.Applicative ((<*>))

-- | checkSort verify if a given list is sorted
-- Point-free version is hard to find BUT is redable.
-- Level: Medium
--
-- Examples:
--
-- >>> checkSort ([1..10] :: [Int])
-- True
--
-- >>> checkSort ([1,3,2] :: [Int])
-- False
--
-- >>> checkSort []
-- True
--
checkSort :: Ord a => [a] -> Bool
checkSort =  and . uncurry (zipWith (<=)) . (id &&& tail)

-- Another popular proposition that avoid uncurry
-- (and is more elegant in my opinion, although it may be less readable
-- for newcomers
checkSort' :: Ord a => [a] -> Bool
checkSort' =  and . (zipWith (<=) <*> tail)
