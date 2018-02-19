module Y2018.M02.D19.Solution where

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

import Data.List (permutations)
import Data.Set (Set)
import qualified Data.Set as Set

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege (assert)

-- okay, we could compute all the primes, ... but nah.

primes :: Set Int
primes =
   Set.fromList [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 
   59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 
   139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 
   227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 
   311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 
   401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 
   491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 
   599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 
   683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 
   797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 
   887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997]

permutablePrimes :: Int -> Set (Set Int)
permutablePrimes n =
   Set.fromList (takeWhile (< n) (Set.toList primes)                   >>=
   assert (all (`Set.member` primes)) . map read . permutations . show >>=
   return . Set.fromList)

{--
>>> last (Set.toList $ permutablePrimes 200)
fromList [199,919,991]

How many permutable primes sets are there up to 100? up to 1000?

>>> map Set.toList (Set.toList (permutablePrimes 100))
[[2],[3],[5],[7],[11],[13,31],[17,71],[37,73],[79,97]]
>>> map Set.toList (Set.toList (permutablePrimes 1000))
[[2],[3],[5],[7],[11],[13,31],[17,71],[37,73],[79,97],[113,131,311],
 [199,919,991],[337,373,733]]
--}
