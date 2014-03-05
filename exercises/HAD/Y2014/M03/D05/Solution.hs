module HAD.Y2014.M03.D05.Solution where

import Data.Monoid (Endo (..), appEndo)
import Data.Foldable (foldMap)

-- | chainEndos chain a list of endofunctions to create a new one
-- Point-free version is feasible and readable.
-- Level: Easy
--
-- Examples:
--
-- >>> chainEndos [(+1),(*3)] 2
-- 7
-- >>> chainEndos [('h':),('e':)] "llo"
-- "hello"
--
-- >>> chainEndos [] (12 :: Int)
-- 12
--
chainEndos :: [a->a] -> a -> a
chainEndos = appEndo . foldMap Endo

chainEndos' :: [a->a] -> a -> a
chainEndos' = foldr (.) id
