module HAD.Y2014.M04.D04.Solution where

import Data.List (nub)

-- | countFigures count the different figures that composes a number
--
-- Examples:
--
-- >>> countFigures 1
-- 1
-- >>> countFigures 1000000
-- 2
-- >>> countFigures 123
-- 3
-- >>> countFigures (-12)
-- 2
-- >>> countFigures 1234567890
-- 10
-- >>> countFigures 00001
-- 1
-- >>> countFigures 0
-- 1
--
countFigures :: Integral a => a -> Int
countFigures =
  max 1 . length . nub . map snd 
  . tail . takeWhile (/=(0,0)) . iterate (flip divMod 10 . fst)
  . flip (,) 1 . abs

-- lazy version
countFigures' :: Integral a => a -> Int
countFigures' = length . nub . show . fromIntegral . abs
