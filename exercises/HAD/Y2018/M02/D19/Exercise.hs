module Y2018.M02.D19.Exercise where

{--
From Fermat's Library @fermatslibrary

https://twitter.com/fermatslibrary/status/964139748230160389

A permutable prime is a prime number which, in a given base, can have its 
digits' positions switched through any permutation and still be a prime number. 
The largest known is (10^278343-1)/9.

991 is prime
199 is prime
919 is prime

Find the permutable primes in range n
--}

import Data.Set (Set)

permutablePrimes :: Int -> Set (Set Int)
permutablePrimes n = undefined

{--
>>> last (Set.toList $ permutablePrimes 200)
fromList [199,919,991]

How many coprime sets are there up to 100? up to 1000?
--}
