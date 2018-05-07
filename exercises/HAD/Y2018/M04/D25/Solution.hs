module Y2018.M04.D25.Solution where

{--
Today is a fun little number theory diversion brought to you by @fermatslibrary

https://twitter.com/fermatslibrary/status/989124910223101952

The last digit of any integer n^5 is n itself

Proof: https://www.youtube.com/watch?v=ZQUTV9or98s

Neat.

Proof this for the first 100 Integers:
--}

n5isn :: Integer -> Bool
n5isn n = (n ^ 5) `mod` 10 == n `mod` 10

{--
The function n5isn takes n and compares the last digit of n to the last digit
of n^5, returning true if they are equal. This is base 10, of course.

>>> map n5isn [1..5]
[True,True,True,True,True]
--}

upto :: Integer -> Bool
upto = all n5isn . enumFromTo 1

-- The function upto returns True if the first m integers follow the n5isn rule

{--
>>> upto 100
True
--}
