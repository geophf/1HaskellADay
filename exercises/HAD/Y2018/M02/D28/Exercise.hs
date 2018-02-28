module Y2018.M02.D28.Exercise where

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

-- so we need pairs of different squares, Casey being the younger one.

pairs0squares :: [(Int, Int)]
pairs0squares = undefined

-- presumably only up to 100, but ... m'kay

-- then we need a pair0square that, four years from now, one doubles the other

doublesInFourYears :: [(Int, Int)] -> Bool
doublesInFourYears pair = undefined

-- then we need to stitch those two together to get our solution

caseysMom :: [(Int, Int)]
caseysMom = undefined
