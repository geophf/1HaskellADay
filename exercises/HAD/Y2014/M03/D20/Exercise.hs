module HAD.Y2014.M03.D20.Exercise where

-- | addNoise
-- add "noise" to a list of number: each number is modified by a
-- random value between -x and x (where x is one of the parameter of the
-- function.
--
-- Quite easy
-- I didn't try it pointfree
-- I didn't try to find a one-liner
--
-- The function signature follows the idea of the methods in the System.Random
-- module: given a standard generator, you returns the modified list and the
-- generator in an altered state.
--
addNoise :: (Num a, Random a) => a -> [a] -> StdGen -> ([a], StdGen)
addNoise = undefined
