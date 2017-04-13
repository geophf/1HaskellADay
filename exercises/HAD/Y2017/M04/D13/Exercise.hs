module Y2017.M04.D13.Exercise where

-- below import available via 1HaskellADay git repository

import Data.QBit

{--
Another puzzler from the Mensa Genius Quiz-a-Day Book by Dr. Abbie Salny.

The following multiplication problem uses every digit from 0 to 9 exactly once

     7 x x
       4 x
 ---------
 x x x x x

Fill in the missing numbers.
--}

type Digit = QBit Int

multiplicationPuzzle :: [Digit] -> [Digit] -> [Digit] -> [([Int], [Int], [Int])]
multiplicationPuzzle x y z = undefined

-- Define multiplicationPuzzle so that x * y == z is true
