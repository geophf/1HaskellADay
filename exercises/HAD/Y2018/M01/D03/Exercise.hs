module Y2018.M01.D03.Exercise where

{--
Now.

For something COMPLETELY different:

From the wikipedia entry on Baron Munchhausen:

https://en.wikipedia.org/wiki/Baron_Munchausen

We have the following note on Munchausen numbers:

... the mathematical term "Munchausen number", coined by Daan van Berkel in 
2009 to describe numbers whose digits, when raised to their own powers, can be 
added together to form the number itself 

for example, 3435 = 3^3 + 4^4 + 3^3 + 5^5

Write a munchausen number verifier.
--}

isMunchausen :: Integer -> Bool
isMunchausen n = undefined 

{--
>>> isMunchausen 3435
True
>>> isMunchausen 4
False
--}

{-- BONUS -----------------------------------------------------------------

Here's the thing.

3435 is the only Munchausen number, base ten, but there are other Munchausen
numbers in other bases.

Write a base-n Munchausen number verifier. Then, with that, what is the first
Munchausen number base 9?
--}

isMunchausen' :: Int -> Integer -> Bool
isMunchausen' base n = undefined
