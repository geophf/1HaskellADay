module Y2017.M03.D17.Solution where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Function (on)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

-- below imports available via 1HaskellADay git repository

import Analytics.Theory.Number.Prime
import Control.DList
import Control.List (takeout)
import qualified Data.MultiMap as MM

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
permutationOf = (==) `on` sort . show

{--
>>> permutationOf 1013 1103
True

>>> permutationOf 1013 1123
False

But, for me, trolling the primes list with permutationOf is a canarde. Instead
simply insert the primes into a multimap keyed by their Set.fromList values.
--}

indexedPrimes :: Map String [Prime]
indexedPrimes =
   Map.map dlToList . MM.store . MM.fromList dl' . map prime2pair 
       $ rangedPrimes 1000 9999

rangedPrimes :: Integer -> Integer -> [Prime]
rangedPrimes from to = takeWhile (< to) $ dropWhile (< from) primes

prime2pair :: Prime -> (String, Prime)
prime2pair = sort . show &&& id

fudgeRipple :: Map a [Prime] -> [[Prime]]
fudgeRipple = filter ((> 2) . length) . Map.elems

sameδ :: [[Prime]] -> [[[Prime]]]
sameδ = filter (not . null)
      . map (\list -> takeout list >>= \(a, l1)              ->
                      takeout (filter (> a) l1) >>= \(b, l2) ->
                      takeout (filter (> b) l2) >>= \(c, _)  ->
                      guard (b - a == c - b)                 >> 
                      return [a,b,c])

-- sameδ (fudgeRipple indexedPrimes) includes [1487,4817,8147] so, yay!

mashPermPrimes :: [Prime] -> String
mashPermPrimes [p1, p2, p3] = concatMap show [p1, p2, p3]

-- So, other than "148748178147", what's your answer?

{--
>>> map (map mashPermPrimes) (sameδ (fudgeRipple indexedPrimes))

includes [["148748178147"],...]
--}
