module Y2018.M10.D31.Solution where

{--
Happy Halloween, Haskellers!

This arithmetic puzzle has been making the rounds today. In words (image on
twitter):
--}

import Control.Monad

ghost, pumpkin, bats :: Int
ghost = 4
pumpkin = 8
bats = 3
-- answer = undefined

equation1, equation2, equation3 :: Bool
equation1 = ghost + ghost + ghost == 12
equation2 = ghost + pumpkin + pumpkin == 20
equation3 = pumpkin + bats + bats == 14
-- equation4 = pumpkin + ghost + bats == answer

{-- 
for all the equations 

1. they are True, (Dependent Types would be nice here)
2. what are the values of ghost, pumpkin, bats, and answers?
3. how would you go about solving these equations?

... actually, this is rather easy: 3 ghosts == 12, so ghost = 4 and the
rest falls out by substitution.
--}

answer :: [Int]
answer = guard equation1 >> guard equation2 >> guard equation3 >>
         return (pumpkin + ghost + bats)

{--
>>> answer
[15]
--}
