module Y2017.M03.D17.Exercise where

-- below import available via 1HaskellADay git repository

import Analytics.Theory.Number.Prime

{--
url: https://projecteuler.net/problem=49

Prime permutations
Problem 49

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases 
by 3330, is unusual in two ways: 

(i) each of the three terms are prime, and, 
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this 
sequence?
--}

permutationOf :: Prime -> Prime -> Bool
permutationOf p1 p2 = undefined

{--
>>> permutationOf 1013 1103
True

>>> permutationOf 1013 1123
False
--}

mashPermPrimes :: [Prime] -> String
mashPermPrimes [p1, p2, p3] = undefined

-- So, other than "148748178147", what's your answer?
