module HAD.Y2014.M02.D28.Solution where

import Control.Applicative

-- | use elements of the first lists to zip elements
-- of the second and third lists
--
-- Examples
-- >>> zipBinary [(+), (*), (*)] [2,2,2] [4,4,4]
-- [6,8,8]
-- >>> zipBinary [(+)] [2,2,2] [4,4,4]
-- [6]
-- >>> zipBinary (cycle [(+), (*)]) [1 .. 4] [2..5]
-- [3,6,7,20]
zipBinary :: [a->b->c] -> [a] -> [b] -> [c]
zipBinary = zipWith3 id

zipBinary' :: [a->b->c] -> [a] -> [b] -> [c]
zipBinary' ops xs ys = getZipList $ ZipList ops <*> ZipList xs <*> ZipList ys
