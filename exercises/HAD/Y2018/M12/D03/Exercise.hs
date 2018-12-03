module Y2018.M12.D03.Exercise where

{--
The KAYAK puzzle.

A 30 x 30 grid contains a random dispersion of the letters A, K, and Y.

1. generate this grid; print it
2. Does this grid contain the word 'KAYAK' horizontally, vertically, or
   BONUS: diagonally?
3. BONUS: How many occurances of the word 'KAYAK' are in this grid?

Hint: to generate a random number from 1 to 3

>>> randomRIO (1,3)
3
--}

import Data.Array
import System.Random

type Size = Int
type Grid = Array (Int,Int) Char

aky :: Array Int Char
aky = listArray (1,3) "AKY"

grid :: Size -> IO Grid
grid sz = undefined

printGrid :: Grid -> IO ()
printGrid grid = undefined

hasKayakinRowOrColumn :: Grid -> Bool
hasKayakinRowOrColumn grid = undefined

-- BONUS -------------------------------------------------------

hasKayakinDiagonals :: Grid -> Bool
hasKayakinDiagonals grid = undefined

kayaksCount :: Grid -> Int
kayaksCount grid = undefined
