module Y2017.M02.D22.Exercise where

-- below import available from 1HaskellADay git repository

import Analytics.Math.Combinatorics hiding (fibr, fibonacci)
import Y2017.M02.D21.Solution (recur)

{--
As I mentioned at the bottom of Analytics.Math.Combinatorics, fibonacci has
two major problems:

1. fibonacci (-1) never terminates
2. fibonacci (10 ^ 100 ^ 100) consumes a tremendous amount of memory

Both of these problems are unnecessary as they are both avoidable with a
better definition of fibonacci (implying: a better definition of fibr)

This is all to say, today's Haskell problem:

write a definition of fibonacci that is

1. well-behaved for all Integers (including negative integers)
2. has constant (that is: non-growing) memory footprint even as n becomes large
3. is O(n), so, none of this exponential-time stuff allowed
--}

fibonacci' :: Integer -> Integer
fibonacci' n = undefined

-- hint: look at, ... possibly to improve, ... fibr definition.

-- What are the fibonacci values for n <- [-12, 6, 25, 105, 1027]?

{-- BONUS -----------------------------------------------------------------

Another, subtle, problem with fibonacci is that it is a specific solution to
a more general problem. If you look at, e.g., the url:

http://rosalind.info/problems/fib/

we see that fib is a specific expression of a recurrence relation, one that
I explored further in the above import. Okay!

So, define fibonacci in terms of a recurrence relation.
--}

fibonacci :: Integer -> Integer
fibonacci n = undefined

-- What, again, are the values for (map fibonacci [-3, 7, 19, 99, 2022])?
-- What is the memory footprint for finding these values? How long did it take?
