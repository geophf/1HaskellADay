module HAD.Y2014.M03.D12.Exercise where

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
localMax = undefined
