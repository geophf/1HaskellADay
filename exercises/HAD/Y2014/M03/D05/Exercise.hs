module HAD.Y2014.M03.D05.Exercise where

-- | chainEndos chain a list of endomorphism to create a new one
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
chainEndos = undefined
