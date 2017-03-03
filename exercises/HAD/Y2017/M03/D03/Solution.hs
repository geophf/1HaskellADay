module Y2017.M03.D03.Solution where

import Data.Ratio

-- Below imports available from 1HaskellADay git repository

import Analytics.Math.Combinatorics (factorial, choose)
import Control.Logic.Frege ((<<-))

{--
url: http://rosalind.info/problems/pper/

Partial Permutations solved by 2239 as of February 28th, 2017

Partial Gene Orderingsclick to collapse

Similar species will share many of the same genes, possibly with modifications. 
Thus, we can compare two genomes by analyzing the orderings of their genes, then
inferring which rearrangements have separated the genes.

In “Enumerating Gene Orders”, we used permutations to model gene orderings. Yet 
two genomes will have evolved along their own separate paths, and so they won't 
share all of the same genes. As a result, we should modify the notion of 
permutation in order to quantify the notion of partial gene orderings.

Problem

A partial permutation is an ordering of only k objects taken from a collection 
containing n objects (i.e., k <= n). For example, one partial permutation of 
three of the first eight positive integers is given by (5,7,2)

The statistic P(n,k) counts the total number of partial permutations of k 
objects that can be formed from a collection of n objects. Note that P(n,n)
is just the number of permutations of n objects, which we found to be equal to 

n!=n(n−1)(n−2)...(3)(2)

in “Enumerating Gene Orders”.

Given: Positive integers n and k such that 100 => n > 0 and 10 => k > 0

Return: The total number of partial permutations P(n,k), modulo 1,000,000.
--}

sample :: String
sample = "21 7"

result :: Integer
result = 51200

partial_permutation :: Integer -> Integer -> Rational
partial_permutation n k = factorial n / factorial (n-k)

pper :: Integer -> Integer -> Integer
pper = (`mod` 1000000) . numerator <<- partial_permutation

{--
>>> pper 21 7
51200
--}

-- BONUS -----------------------------------------------------------------

-- Now, with this definition, what is the pper value of:

biggun :: String
biggun = "96 9"

{--
>>> pper 96 9
934400
--}

{-- BONUS-BONUS! ----------------------------------------------------------

rewrite the Analytics.Math.Combinatorics.choose-function in terms of
partial_permutation
--}

choosey :: Integer -> Integer -> Rational
choosey n k = partial_permutation n k / factorial k

-- because choosey mothers choose JIF!

-- verify that choosey == choose for [(n,k) <- [1..100]] ...ish.

{--
>>> and ([1..100] >>= \k -> [k .. 100] >>= \n -> return (choose n k == choosey n k))
True
--}

-- partial_permutation being rolled into Analytics.Math.Combinators
