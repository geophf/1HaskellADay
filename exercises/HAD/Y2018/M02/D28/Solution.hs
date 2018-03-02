module Y2018.M02.D28.Solution where

{--
Another Mensa problem from:

https://www.scientificamerican.com/article/can-you-solve-these-mensa-puzzles5/

(1) Casey and his mom are both celebrating birthdays and turning ages that are
    perfect squares. In four years, Casey's age will be exactly half his 
    mother's age. How old are they now?

Hm. Welp. We know some facts:

1. Casey and Mom are different ages
2. Now Casey's and Mom's ages are perfect squares.
--}

-- below import available from 1HaskellADay git repository

import Control.Logic.Frege ((-|))

-- so we need pairs of different squares, Casey being the younger one.

pairs0squares :: [(Int, Int)]
pairs0squares =
   [1..9]         >>= \x -> let sqx = x * x in
   [succ x .. 10] >>= \y -> let sqy = y * y in
   sqy - sqx > 9 -| [(sqx, sqy)]

{-- first-...ish try:

   [(x * x,y * y) | x <- [1..9],
                    y <- [if x < 3 then 3 else succ x .. 10]]

nope!
--}

-- The mom has to be at least ... 9? when Casey was born because biology.

-- presumably only up to 100, but ... m'kay

-- then we need a pair0square that, four years from now, one doubles the other

doublesInFourYears :: (Int, Int) -> Bool
doublesInFourYears (kid, mom) = 2 * (kid + 4) == mom + 4

-- then we need to stitch those two together to get our solution
 
caseysMom :: [(Int, Int)]
caseysMom = filter doublesInFourYears pairs0squares

{--
>>> caseysMom 
[(16,36)]

Yup. So Casey's mom was twenty when Casey was born.
--}
