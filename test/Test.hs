module Test 
  ( checkModule
  , check
  , checkCurrent
  )
  where

import Data.Time
import Text.Printf
import Test.DocTest

-- Runs the doctest of the Exercise.hs file of a given module.
-- A bit painful to write, it's easier to use checkByDate.
-- Example:
-- 
-- check "HAD.Y2014.M02.D24"
checkModule :: String -> IO ()
checkModule = doctest . ("-isrc":) . return


-- Runs the doctest of Exercise.hs file of te given date module.
-- Example:
-- 
-- checkByDate 2014 02 24
check :: Int -> Int -> Int -> IO ()
check = let buildModule = printf "HAD.Y%4d.M%02d.D%02d.Exercise"
  in ((checkModule .) .) . buildModule


-- Runs the doctest of today's Exercise.hs file.
-- Example:
-- 
-- checkByDate 2014 02 24
checkCurrent :: IO ()
checkCurrent = do
  (y, m, d) <- getCurrentDay
  check (fromInteger y) m d

getCurrentDay :: IO (Integer, Int, Int)
getCurrentDay = fmap (toGregorian . utctDay) getCurrentTime
