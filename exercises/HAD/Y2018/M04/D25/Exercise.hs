module Y2018.M04.D25.Exercise where

{--
Today is a fun little number theory diversion brought to you by @fermatslibrary

https://twitter.com/fermatslibrary/status/989124910223101952

The last digit of any integer n^5 is n itself

Proof: https://www.youtube.com/watch?v=ZQUTV9or98s

Neat.

Proof this for the first 100 Integers:
--}

n5isn :: Integer -> Bool
n5isn n = undefined

{--
The function n5isn takes n and compares the last digit of n to the last digit
of n^5, returning true if they are equal. This is base 10, of course.
--}

upto :: Integer -> Bool
upto m = undefined

-- The function upto returns True if the first m integers follow the n5isn rule
