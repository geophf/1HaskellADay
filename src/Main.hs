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
  | CheckSolution Int Int Int
  | Read Int Int Int
  | ReadCurrent
  | ReadSolution Int Int Int
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

checkDateCommand :: Maybe Command -> Command
checkDateCommand = fromMaybe $ InvalidCommand notice
    


readCommand :: [String] -> Command
readCommand ["check","current"] = CheckCurrent
readCommand ("check":xs) =
  checkDateCommand $ readDateCommand Check xs
readCommand ("checkSolution":xs) =
  checkDateCommand $ readDateCommand CheckSolution xs
readCommand ["read","current"] = ReadCurrent
readCommand ("read":xs) = checkDateCommand $ readDateCommand Read xs
readCommand ("readSolution":xs) =
  checkDateCommand $ readDateCommand ReadSolution xs
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
execCommand (ReadSolution y m d) = readSolution y m d
execCommand (InvalidCommand s) = putStrLn s
execCommand Help = putStrLn notice

notice = unlines
  [ "usage: 1had <command> [<args>]"
  , ""
  , "The currently available commands are the following:"
  , ""
  , "check <year> <month> <day> check your proposition for a given day"
  , "check current              check your proposition for a given day"
  , "read  <year> <month> <day> read the exercise content for a given day"
  , "read  current              read the exercise content for a given day"
  ]
