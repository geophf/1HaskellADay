module Main
  ( main
  ) where

import HAD
import Control.Arrow
import Data.Maybe
import Text.Printf
import System.Environment

main :: IO ()
main = do
 a <- getArgs
 execCommand . readCommand $ a

data Command
  = Check Int Int Int
  | CheckCurrent
  | Read Int Int Int
  | ReadCurrent
  | CheckSolution Int Int Int
  | InvalidCommand String
  | Help
  deriving Eq

readInt :: String -> Maybe Int
readInt s = case readsPrec 1 s of
  [(x,"")] -> Just x
  []       -> Nothing

readDateCommand :: (Int -> Int -> Int -> Command) -> [String] -> Maybe Command
readDateCommand cmd [xs,ys,zs] = do
    y <- readInt xs
    m <- readInt ys
    d <- readInt zs
    return $ cmd y m d
readDateCommand cmd _ = Nothing

checkDateCommand :: String -- command name
                 -> Maybe Command
                 -> Command
checkDateCommand =
  fromMaybe
    . InvalidCommand . printf "usage: 1had %s <year> <month> <day>"
    


readCommand :: [String] -> Command
readCommand ("check":xs) = checkDateCommand "check" $ readDateCommand Check xs
readCommand ("checkSolution":xs) =
  checkDateCommand "checkSolution" $ readDateCommand CheckSolution xs
readCommand ["checkCurrent"] = CheckCurrent
readCommand ("checkCurrent":_) = InvalidCommand ("usage: 1had checkCurrent")
readCommand ("read":xs) = checkDateCommand "read" $ readDateCommand Read xs
readCommand ["readCurrent"] = ReadCurrent
readCommand ("readCurrent":_) = InvalidCommand ("usage: 1had readCurrent")
readCommand ("help":_) = Help
readCommand _ = InvalidCommand "unknown command"

instance Read Command where
  readsPrec _ = return . (id &&& const "") . readCommand . words

execCommand :: Command -> IO ()
execCommand (Check y m d) = check y m d
execCommand CheckCurrent = checkCurrent
execCommand (CheckSolution y m d) = checkSolution y m d
execCommand (Read y m d) = readExercise y m d
execCommand ReadCurrent = readCurrentExercise
execCommand (InvalidCommand s) = putStrLn s
execCommand Help = putStrLn notice

notice = unlines
  [ "usage: 1had <command> [<args>]"
  , ""
  , "The currently available commands are the following:"
  , ""
  , "check        check your proposition for a given day"
  , "checkCurrent check your proposiiton for the current day"
  , "read         read the exercise content for a given day"
  , "readCurrent  read tzhe exercise of the current day"
  ]
