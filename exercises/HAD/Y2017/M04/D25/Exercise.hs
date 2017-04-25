module Y2017.M04.D25.Exercise where

-- below import available via 1HaskellADay git repository

import Data.QBit

import Y2017.M04.D13.Exercise

{--
From the Mensa Genuius Quiz-a-Day Book by Dr. Abbie F. Salny.

One can replace the letters with numbers so the addition will be correct
numerically. (Hint K = 9)

      M O M
      M O M
  +     N O
  ---------
    B O O K

hint-hint: You've looked at solving this kind of problem before (see above)
--}

asNum :: [Digit] -> Int
asNum = undefined

momMomNoBook :: [Digit] -> [Digit] -> [Digit] -> [(Int, Int, Int)]
momMomNoBook mom no book = undefined

-- sum [mom, mom, no] == book ... simple, eh?

-- Also recall that M, N, B cannot be 0 and given K == 9, O has a certain property
