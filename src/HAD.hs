module HAD 
  ( check
  , checkSolution
  , edit
  , readExercise
  , readSolution
  , module HAD.Date
  )
  where

import Data.List (intercalate)
import Text.Printf (printf)
import Test.DocTest (doctest)
import System.FilePath.Posix (joinPath)
import System.Cmd (rawSystem)
import HAD.Date

type DateCommand a = (Int, Int, Int) -> a

-- Runs the doctest of Exercise.hs file of te given date module.
-- Example:
-- 
-- >>> check =<< date 2014 02 24
check :: DateCommand (IO ())
check = checkFile "Exercise"

-- Check the solution of a given Day
checkSolution :: DateCommand (IO ())
checkSolution = checkFile "Solution"

-- Read an exercise content
readExercise :: DateCommand (IO ())
readExercise date = do
  s <- readFile . exercisePath $ date
  putStrLn $ '\n':s

-- Read an solution content
readSolution :: DateCommand (IO ())
readSolution date = do
  s <- readFile . solutionPath $ date
  putStrLn $ '\n':s

-- Edit the exercise of a given day
edit :: String -- Editor
     -> DateCommand (IO ())
edit ed date = do
  rawSystem ed $ return . exercisePath $ date 
  return ()

-- helper build a solution FilePath
solutionPath :: DateCommand FilePath
solutionPath = elementPath "Solution.hs"

-- helper build an exercise FilePath
exercisePath :: DateCommand FilePath
exercisePath = elementPath "Exercise.hs"

-- Helper to access a file in an exercise directory
elementPath :: String -> DateCommand FilePath
elementPath name =
  joinPath . ("exercises":) . (++ [name]) . modules

-- Check the given file in the directory that corespond to the given day
checkFile :: String -> DateCommand (IO ())
checkFile fn = let
  modules2module = intercalate "." . (++ [fn])
  in checkModule . modules2module . modules

-- Runs the doctest of the Exercise.hs file of a given module.
-- A bit painful to write, it's easier to use checkByDate.
-- Example:
-- 
-- check "HAD.Y2014.M02.D24"
checkModule :: String -> IO ()
checkModule = doctest . ("-iexercises":) . return

-- helper that build modules list
modules :: DateCommand [String]
modules (y,m,d) = "HAD": zipWith printf ["Y%4d", "M%02d", "D%02d"] [y,m,d]
