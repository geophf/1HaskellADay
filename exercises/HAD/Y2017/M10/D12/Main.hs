module Main where

-- just a little module to say, Hey! this app is a Haskell app. It's special!
-- ... and you are, too, punkin. D'AWWWWW! <3 <3

import System.Environment (getArgs)

-- for creating ~/bin/scanner

import Y2017.M10.D12.Solution (main')

main :: IO ()
main = getArgs >>= main'

-- it may be small, but an app is an app, no matter how small.
