module HAD.Y2014.M04.D02.Solution where

import Control.Monad (mfilter)

-- | update update the nth element of a list
-- if the index is not a valid index, it leaves the list unchanged
--
-- Examples
--
-- >>> update (-2) 10 [0..4]
-- [0,1,2,3,4]
--
-- >>> update 6 10 [0..4]
-- [0,1,2,3,4]
--
-- >>> update 2 10 [0..4]
-- [0,1,10,3,4]
--
update :: Int -> a -> [a] -> [a]
update i v = zipWith (maybe (const v) (const id) . mfilter (/= i) . Just) [0..]
