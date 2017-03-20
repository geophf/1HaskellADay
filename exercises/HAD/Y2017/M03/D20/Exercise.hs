module Y2017.M03.D20.Exercise where

-- below import available via 1HaskellADay git repository

import Analytics.Theory.Number.Prime

{--
Okay, today's Haskell problem is formulated with an eye toward the Project Euler
problem 50: longest consecutive prime sum. We're not going to solve that problem
today, but we will look at a sub-problem of that.

The larger problem, that we are not solving, is to find the longest sequence
of consecutive primes that sum to some prime, p, less than solve value n.

We're not solving that larger problem; instead, we'll solve a smaller problem,
which is as follows: find the largest prime, p, below some value, n that has
the longest run of consecutive primes starting with 2.

e.g.:

The prime 41 can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below 
one-hundred.

write a function that finds this prime (41) and the sequence of consecutive
primes starting with 2 that sums to it:
--}

consecutivePrimeSumBelow :: Integer -> [(Prime, [Prime])]
consecutivePrimeSumBelow n = undefined

{--
>>> head (consecutivePrimeSumBelow 100)
(41,[2,3,5,7,11,13])

Okay, what is the first prime with consecutive primes that sum to it below 1000?

>>> head (consecutivePrimeSumBelow 1000)

What is the first prime, below one million, can be written as the sum of 
consecutive primes?

>>> head (consecutivePrimeSumBelow 1000000)
--}
