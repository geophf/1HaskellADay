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
  | Edit String Int Int Int
  | Help
  | InvalidCommand
  deriving Eq

readInt :: String -> Maybe Int
readInt s = case readsPrec 1 s of
  [(x,"")] -> Just x
  []       -> Nothing

readDateCommand :: (Int -> Int -> Int -> Command)
                -> String
                -> String
                -> String
                -> Maybe Command
readDateCommand cmd xs ys zs = do
    y <- readInt xs
    m <- readInt ys
    d <- readInt zs
    return $ cmd y m d

readCommandName :: String -> (Int -> Int -> Int -> Command)
readCommandName "check" = Check
readCommandName "read"  = Read
readCommandName "checkSolution" = CheckSolution
readCommandName "readSolution"  = ReadSolution

checkDateCommand :: Maybe Command -> Command
checkDateCommand = fromMaybe $ InvalidCommand
    

readCommand :: [String] -> Command
readCommand ["check","current"] = CheckCurrent
readCommand ["read","current"] = ReadCurrent
readCommand ("help":_) = Help
readCommand [cmd,y,m,d] =
  checkDateCommand $ readDateCommand (readCommandName cmd) y m d
readCommand ["edit",ed,y,m,d] =
  checkDateCommand $ readDateCommand (Edit ed) y m d
readCommand _ = InvalidCommand

instance Read Command where
  readsPrec _ = return . (id &&& const "") . readCommand . words

execCommand :: Command -> IO ()
execCommand CheckCurrent = checkCurrent
execCommand ReadCurrent = readCurrentExercise
execCommand (Check y m d) = check y m d
execCommand (Read y m d) = readExercise y m d
execCommand (CheckSolution y m d) = checkSolution y m d
execCommand (ReadSolution y m d) = readSolution y m d
execCommand (Edit e y m d) = edit e y m d
execCommand Help = putStrLn notice
execCommand InvalidCommand = putStrLn ("Invalid command\n\n" ++ notice)

notice = unlines
  [ "usage: 1had <command> [<args>]"
  , ""
  , "The currently available commands are the following:"
  , ""
  , "check <year> <month> <day>          check your proposition for a given day"
  , "check current                       check your proposition for today"
  , "read  <year> <month> <day>          read a given exercise"
  , "read  current                       read today's exercise"
  , "edit  <editor> <year> <month> <day> edit a given exercise"
  ]
