module HAD.Y2114.M03.D27.Exercise where

-- | compact Compact a list of values with the following rules:
-- - Nothing are removed
-- - If two remaining onsecutive values are equal,
--     they are replaced by one value equal to the successor of those values
-- - the previous rule is not recursive
-- - Other values are kept
--
-- Examples
--
-- >>> compact [Just 1, Nothing, Just 1, Just 2, Just 4, Just 3, Just 3]
-- [2,2,4,4]
--
-- >>> compact [Nothing, Nothing, Nothing]
-- []
--
-- >>> compact []
-- []
--
-- >>> compact $ map Just "hello"
-- "hemo"
--
-- prop> [succ x] == (compact . replicate 2 . Just) (x :: Int)
-- prop> [succ x, x] == (compact . replicate 3 . Just) (x :: Int)
-- prop> replicate 2 (succ x) == (compact . replicate 4 . Just) (x :: Int)
--
compact :: (Enum a, Eq a) => [Maybe a] -> [a]
compact = undefined
