module HAD.Y2014.M03.D24.Solution where

import Data.List (groupBy)

-- | squareList builds a list of x lists of size x from a given list of elements
-- If there aren't enough elements, fill the square with the second parameter
-- Examples:
--
-- >>> squareList 2  0 [0..]
-- [[0,1],[2,3]]
--
-- >>> squareList 2 0 [1]
-- [[1,0],[0,0]]
--
-- >>> squareList 3 () $ repeat ()
-- [[(),(),()],[(),(),()],[(),(),()]]
--
squareList :: Int -> a -> [a] -> [[a]] 
squareList x e =
  take x
    . map (map snd)
    . groupBy (const ((/= 0) . fst))
    . zip (cycle [0..x-1])
    . (++ repeat e)
