module Y2018.M11.D22.Solution where

{--
Happy Thanksgiving from the U.S.A.!

From @fermatslibrary:

url: https://twitter.com/fermatslibrary/status/1064519419303550976

A visual proof that
1/4 + 1/4² + 1/4³ + ... = 1/3

⚪⚪⚪⚪⚪⚪⚪⚪
⚪⚫⚪⚪⚪⚪⚪⚪
⚪⚪⚫⚫⚪⚪⚪⚪
⚪⚪⚫⚫⚪⚪⚪⚪
⚪⚪⚪⚪⚫⚫⚫⚫
⚪⚪⚪⚪⚫⚫⚫⚫
⚪⚪⚪⚪⚫⚫⚫⚫
⚪⚪⚪⚪⚫⚫⚫⚫

Prove this.

How many terms of fourths to be within 0.1 of 1/3? 0.01? 0.001?
--}

fourthThird :: Int -> Double
fourthThird n = 1/3 - ft n 1

-- fourthThird gives the difference of the sum of fourths vs 1/3.

ft :: Int -> Double -> Double
ft 0 _ = 0
ft n x = 1 / 4 ** x + ft (pred n) (succ x)

{--
>>> ft 1 1
0.25
>>> ft 2 1
0.3125
>>> ft 3 1
0.328125
>>> ft 4 1
0.33203125
>>> ft 5 1
0.3330078125
--}

howManyTerms :: Double -> Int
howManyTerms = hmt 1

hmt :: Int -> Double -> Int
hmt n err = if fourthThird n < err then n else hmt (succ n) err

{--
>>> howManyTerms 0.1
1
>>> howManyTerms 0.01
3
>>> howManyTerms 0.001
5
>>> howManyTerms 0.0001
6
--}
