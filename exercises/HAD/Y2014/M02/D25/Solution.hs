module HAD.Y2014.M02.D25.Solution where

import Control.Arrow
import Data.List

-- Implement a variation of the RLE algorithm

-- | Compress a list with RLE
--
-- Examples:
--
-- >>> compress "Hello"
-- [('H',1),('e',1),('l',2),('o',1)]
--
-- >>> compress [1,1,1,1,1]
-- [(1,5)]
compress :: Eq a => [a] -> [(a, Int)]
compress = map (head &&& length) . group

-- | Expand a list with RLE
--
-- Examples:
--
-- >>> expand [('H',1),('e',1),('l',2),('o',1)]
-- "Hello"
--
-- >>> expand [(1,5)]
-- [1,1,1,1,1]
expand :: [(a, Int)] -> [a]
expand = (>>= uncurry (flip replicate))

-- It should verify
-- prop> (expand . compress) xs == (xs :: String)
