module HAD.Y2014.M04.D15.Solution where

import Data.List

-- $setup
-- >>> import Test.QuickCheck

{- | lowestFreeInt
   Find the lowest non-negative integer not in the list

   (Thanks to @clementd for this one)

   Example:

   >>> lowestFreeInt [0..10]
   11

   >>> lowestFreeInt [1..10]
   0

   >>> lowestFreeInt $ [0..998] ++ [1000..9999]
   999

   The issue happens when the list is not ordered:
   >>> lowestFreeInt $ reverse $ [0..998] ++ [1000..9999]
   999

-}

-- Shamely taken from "Pearl of Functionnal programming".
lowestFreeInt :: [Int] -> Int
lowestFreeInt xs = minFrom 0 (length xs , xs)
minFrom a (n , xs) | n == 0     = a
                   | m == b - a = minFrom b (n - m, vs)
                   | otherwise  = minFrom a (m , us)
                   where (us , vs) = partition (< b) xs
                         b = a + 1 + n `div` 2
                         m = length us

-- My proposal, which has the same complexity as the sort algorithm it relies on
lowestFreeInt' :: [Int] -> Int
lowestFreeInt' = head . findIndices id . (++ [True]) . zipWith (/=) [0..]

-- The quadratic version
lowestFreeInt'' :: [Int] -> Int
lowestFreeInt'' = head . ([0..] \\)
