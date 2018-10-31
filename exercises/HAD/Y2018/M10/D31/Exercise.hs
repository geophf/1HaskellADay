module Y2018.M10.D31.Exercise where

{--
Happy Halloween, Haskellers!

This arithmetic puzzle has been making the rounds today. In words (image on
twitter):
--}

ghost, pumpkin, bats, answer :: Int
ghost = undefined
pumpkin = undefined
bats = undefined
answer = undefined

equation1, equation2, equation3, equation4 :: Bool
equation1 = ghost + ghost + ghost == 12
equation2 = ghost + pumpkin + pumpkin == 20
equation3 = pumpkin + bats + bats == 14
equation4 = pumpkin + ghost + bats == answer

{-- 
for all the equations 

1. they are True, (Dependent Types would be nice here)
2. what are the values of ghost, pumpkin, bats, and answers?
3. how would you go about solving these equations?
--}
