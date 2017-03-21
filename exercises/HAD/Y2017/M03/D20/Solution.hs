module Y2017.M03.D20.Solution where

-- below import available via 1HaskellADay git repository

import Analytics.Theory.Number.Prime

consecutivePrimeSumBelow :: Integer -> [(Prime, [Prime])]
consecutivePrimeSumBelow n =
   let p = reverse (takeWhile (< n) primes) in cpsb p (tail p)

{--
>>> head (consecutivePrimeSumBelow 100)
(41,[2,3,5,7,11,13])

>>> head (consecutivePrimeSumBelow 1000)
(281,[2,3,5,7,11,13,17,19,23,29,31,37,41,43])

But 281 is NOT the longest sequence of consecutive primes!

What is the longest consecutivePrimeSumBelow 1000000?
(958577,[2,3,5,7,11,13,17,19,...,3853,3863])
--}

cpsb :: [Prime] -> [Prime] -> [(Prime, [Prime])]
cpsb c@(ca:ndidates) p@(pr:imes) | sum p >  ca =      cpsb c        imes
                                 | sum p == ca = (ca, reverse p) : cpsb ndidates imes
                                 | sum p <  ca =      cpsb ndidates p
cpsb [] _                                      = []
cpsb _  []                                     = []

{--
The problem with the above algorithm is my assumption that the longest sequence
always starts with 2. This turns out not to be the case, so this complicates the
algorithm in two ways:

1. If 2 + 3 + 5 ... is not the solution, then 3 + 5 + 7 ... may be, and if THAT
   is the case, then 5 + 7 + 11 ... may be, ad pukem <<- that is Latin.
2. If 2 + 3 + 5 ... is not the solution, then the first number WITH a solution
   as illustrated in 1. above may not be the longest solution.

These two complications may appear to make finding a solution a nightmare,
right? Because it appears

a. that we have to manage solution lengths (we do) and
b. that we have to scan all numbers to find the longest one.

The latter is not necessarily the case. With some smart length management then
some decision-making in our algorithm, finding longest is more complicated than
the original (incorrect) approach but still less of a nightmare than the
generate-and-test-all approach.
--}
