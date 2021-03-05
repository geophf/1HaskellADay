import System.Environment (getArgs)

-- This is the application that calls 'greet' to wish our User well today.

import Y2021.M03.D05.Solution (greet)

main :: IO ()
main = getArgs >>= errorHandleDis

errorHandleDis :: [String] -> IO ()
errorHandleDis [] = putStrLn "Hi. What's your name?"
errorHandleDis n@(_:_) = greet (unwords n)
