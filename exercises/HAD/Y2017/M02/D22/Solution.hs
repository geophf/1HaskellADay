module Y2017.M02.D22.Solution where

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
fibonacci' = fibr [1,0]

fibr :: [Integer] -> Integer -> Integer
fibr [a,b] n | n < 1     = 0
             | n == 1    = a
             | otherwise = fibr [a+b, a] (pred n)

-- What are the fibonacci values for n <- [-12, 6, 25, 105, 1027]?

{--
>>> map fibonacci' [-12, 6, 25, 105, 1027]
[0,8,75025,3928413764606871165730,
 19090686002432645287190774532522845166810196492903222106235434786
 77483094353309455599761134664974009423453689908096879804082641895
 23071599705520104008279051033372582969372429621043562019898499430
 50575285260709682293]

Returned in no time, and very little growth in memory consumption (boxing
the two (large) integer values)
--}

{-- BONUS -----------------------------------------------------------------

Another, subtle, problem with fibonacci is that it is a specific solution to
a more general problem. If you look at, e.g., the url:

http://rosalind.info/problems/fib/

we see that fib is a specific expression of a recurrence relation, one that
I explored further in the above import. Okay!

So, define fibonacci in terms of a recurrence relation.
--}

fibonacci :: Integer -> Integer
fibonacci = flip recur 1

-- What, again, are the values for (map fibonacci [-3, 7, 19, 99, 2022])?
-- What is the memory footprint for finding these values? How long did it take?

{--
>>> map fibonacci [-3, 7, 19, 99, 2022]
[0,13,4181,218922995834555169026,167310648784659280728144836725590014814177
 40079747676087675370408011426011453649538013501424462864154046500947901593
 42993763061932388177841294054658044451407589934236871431466133901233545579
 36785042721146861530732824681611737331775039385078670522766530356710254069
 89498837517631736503027808071321841320104867836063619983051403713130141974
 92869017898957795184267726464050334235713601159942285530988710466965209813
 84561779336]

The new, improved fibonacci along with the recurrence relation function is being
rolled into Analytics.Math.Combinatorics.
--}
