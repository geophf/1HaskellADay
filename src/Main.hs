module Main
  ( main
  ) where

import HAD
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (evalStateT, get, put, StateT)

type Day = (Int, Int, Int)

main :: IO ()
main = do
 args <- getArgs
 case args of
   ("help":_) -> putStrLn notice
   otherwise  -> fromMaybe (putStrLn $ "Invalid command\n\n" ++ notice) $
     evalStateT parseDateCommand args

parseDateCommand :: StateT [String] Maybe (IO ())
parseDateCommand = do
  cmd <- parseDateCommandName
  day <- parseDate
  lift . return $ execDateCommand cmd =<< day
  

parseDateCommandName :: StateT [String] Maybe DateCommand
parseDateCommandName = do
  args <- get
  (cName, xs) <- lift $ parseDateCommandArgs args
  put xs
  return cName

parseDateCommandArgs :: [String] -> Maybe (DateCommand, [String])
parseDateCommandArgs ("read":xs) = return (Read, xs)
parseDateCommandArgs ("read-solution":xs) = return (ReadSolution, xs)
parseDateCommandArgs ("check":xs) = return (Check, xs)
parseDateCommandArgs ("check-solution":xs) = return (CheckSolution, xs)
parseDateCommandArgs ("edit":e:xs) = return (Edit e, xs)
parseDateCommandArgs _ = Nothing

parseDate :: StateT [String] Maybe (IO Day)
parseDate = do
  args <- get
  lift $ parseDateArgs args

parseDateArgs :: [String] -> Maybe (IO Day)
parseDateArgs [xs] | xs `elem` ["current", "last"] = return current
                   | xs == "yesterday"             = return $ xDaysBefore 1
                   | otherwise                     = Nothing
parseDateArgs ["-d",ys]   = do
  d <- fmap toInteger $ readInt ys
  return $ xDaysBefore d
parseDateArgs [xs,ys,zs]  = do
  y <- readInt xs
  m <- readInt ys
  d <- readInt zs
  return . return $ (y, m, d)
parseDateArgs _           = Nothing

readInt :: String -> Maybe Int
readInt s = case readsPrec 1 s of
  [(x,"")] -> Just x
  []       -> Nothing


execDateCommand :: DateCommand -> Day -> IO ()
execDateCommand Check d = check d
execDateCommand CheckSolution d = checkSolution d
execDateCommand Read d = readExercise d
execDateCommand ReadSolution d = readSolution d
execDateCommand (Edit e) d = edit e d


data DateCommand
  = Check
  | CheckSolution
  | Read
  | ReadSolution
  | Edit String
  deriving Eq

notice = unlines
  [ "usage: 1had <command> [<args>]"
  , ""
  , "Example:"
  , "1had read current"
  , "1had edit vi 2014 2 24"
  , ""
  , "The currently available commands are the following:"
  , ""
  , "help                           display this screen"
  , "check          <date>          check your proposition for a given day"
  , "check-solution <date>          check your proposition for a given day"
  , "read           <date>          read a given exercise"
  , "read-solution  <date>          read the solution for a given day"
  , "edit  <editor> <date>          edit a given exercise"
  , ""
  , "Supported Date Format:"
  , "current                        today (or last Friday on Saturday/Sunday)" 
  , "last                           today (or last Friday on Saturday/Sunday)" 
  , "yesterday                      yesterday (or last Friday on Saturday/Sunday)" 
  , "<yyyy> <mm> <dd>               the given day"
  , "-d <int>                       x days before"
  ]
