module Y2017.M03.D03.Exercise where

-- below import available from 1HaskellADay git repository

import Analytics.Math.Combinatorics (choose)

{--
Okay, the problem with the minimumest path is that, okay, we found the shortest
distance for the next nth nucleotide (let's say 'A'), but what if that shortest
distance makes the NEXT step longer than if we had chosen an alternative that
was slightly longer but the combined distance of this one and the next one would
be shorter than defaulting for the shortest distance each time.

And how far a look-ahead guarantees that we will find the summed shortest
distances, and how do we know that the summed shortest distances yield the
longest common subsequence.

I'm going to have to go off and study this problem for a while, put some Deep
Thoughts into this, so we'll come back to this next week.

So, but today is FRIDAY! (icymi) So let's take a breather from minimumest and
look at something entirely different.

url: http://rosalind.info/problems/pper/

Partial Permutations solved by 2239 as of February 28th, 2017

Partial Gene Orderings

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

-- first: what, in general, is the partial permutation of n k?

partial_permutation :: Integer -> Integer -> Rational
partial_permutation n k = undefined

-- now let's take it down so that the solution looks like what the problem
-- request: that is to say, mod 1,000,000 and in integral form

pper :: Integer -> Integer -> Integer
pper n k = undefined

{--
>>> pper 21 7
51200
--}

-- BONUS -----------------------------------------------------------------

-- Now, with this definition, what is the pper value of:

biggun :: String
biggun = "96 9"

{-- BONUS-BONUS! ----------------------------------------------------------

rewrite the Analytics.Math.Combinatorics.choose function in terms of
partial_permutation
--}

choosey :: Integer -> Integer -> Rational
choosey n k = undefined

-- because choosey mothers choose JIF!

-- verify that choosey == choose for [(n,k) <- [1..100]] ...ish.
