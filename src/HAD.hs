module HAD 
  ( check
  , checkSolution
  , checkCurrent
  , edit
  , editCurrent
  , readExercise
  , readSolution
  , readCurrentExercise
  )
  where

import Data.List (intercalate)
import Data.Time
import Text.Printf (printf)
import Test.DocTest (doctest)
import System.FilePath.Posix (joinPath)
import System.Cmd (rawSystem)

-- Runs the doctest of Exercise.hs file of te given date module.
-- Example:
-- 
-- checkByDate 2014 02 24
check :: Int -> Int -> Int -> IO ()
check = checkFile "Exercise"

-- Check the solution of a given Day
checkSolution :: Int -> Int -> Int -> IO ()
checkSolution = checkFile "Solution"

-- Runs the doctest of today's Exercise.hs file.
-- Example:
-- 
-- checkByDate 2014 02 24
checkCurrent :: IO ()
checkCurrent = runToday check

-- Read an exercise content
readExercise :: Int -> Int -> Int -> IO ()
readExercise y m d = do
  s <- readFile $ exercisePath y m d
  putStrLn $ '\n':s

-- Read an exercise content
readSolution :: Int -> Int -> Int -> IO ()
readSolution y m d = do
  s <- readFile $ solutionPath y m d
  putStrLn $ '\n':s

-- Read the exercise of the day
readCurrentExercise :: IO ()
readCurrentExercise = runToday readExercise

-- Edit the exercise of a given day
edit :: String -- Editor
     -> Int -- Year of the exercise
     -> Int -- Month
     -> Int -- Day
     -> IO ()
edit ed y m d = do
  rawSystem ed $ return $ exercisePath y m d
  return ()

-- Edit today's exercise
editCurrent :: String -- Editor
            -> IO ()
editCurrent = runToday . edit


-- helper build a solution FilePath
solutionPath :: Int -> Int -> Int -> FilePath
solutionPath = elementPath "Solution.hs"

-- helper build an exercise FilePath
exercisePath :: Int -> Int -> Int -> FilePath
exercisePath = elementPath "Exercise.hs"

-- Helper to access a file in an exercise directory
elementPath :: String -> Int -> Int -> Int -> FilePath
elementPath name y m d =
  joinPath . ("exercises":) . (++ [name]) $ modules y m d

-- helper to run a function on the given day exercise
runToday :: (Int -> Int -> Int -> IO ()) -> IO ()
runToday f = do
  (y, m, d) <- getCurrentDay
  f (fromInteger y) m d

-- Check the given file in the directory that corespond to the given day
checkFile :: String -> Int -> Int -> Int -> IO ()
checkFile fn y m d = let
  modules2module = intercalate "." . (++ [fn])
  in checkModule . modules2module $ modules y m d

-- Runs the doctest of the Exercise.hs file of a given module.
-- A bit painful to write, it's easier to use checkByDate.
-- Example:
-- 
-- check "HAD.Y2014.M02.D24"
checkModule :: String -> IO ()
checkModule = doctest . ("-iexercises":) . return

-- Helper that get current Day
getCurrentDay :: IO (Integer, Int, Int)
getCurrentDay = fmap (toGregorian . utctDay) getCurrentTime

-- helper that build modules list
modules :: Int -> Int -> Int -> [String]
modules y m d = "HAD": zipWith printf ["Y%4d", "M%02d", "D%02d"] [y,m,d]
