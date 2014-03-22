module HAD.Y2014.M03.D12.Solution where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mFilter)
import Data.Maybe (catMaybes)

-- | localMax Given an entry list, it outputs the its list of local maxima.
-- A Local maximum is a an element greater than its predecessor and than its
-- successor.
--
-- Examples:
--
-- >>> localMax [0 .. 1000]
-- []
--
-- >>> localMax [1000 .. 0]
-- []
--
-- >>> localMax [2,2,1,5,4]
-- [5]
--
-- >>> take 4 . localMax $ [0..] >>= (\y -> [y,y+2])
-- [2,3,4,5]
--
localMax :: Ord a => [a] -> [a]
localMax = let
  checkMax p n = mfilter ((&&) <$> (>p) <*> (>n)) . Just
  in catMaybes . (zipWith3 checkMax <*> drop 2 <*> drop 1)

localMax' :: Ord a => [a] -> [a]
localMax' (x:y:z:xs) | y > x && y > z = y:localMax (z:xs)
                     | otherwise      = localMax (y:z:xs)
localMax' _                           = []
