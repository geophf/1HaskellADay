module Analytics.Theory.Number.Combinatorics where

-- Where you want to get all factorial and stuff

factorial :: Integer -> Rational
factorial = toRational . product . enumFromTo 1

{-- PARTIAL PERMUTATION  --------------------------------------------------

A partial permutation is an ordering of only k objects taken from a collection 
containing n objects (i.e., k <= n). For example, one partial permutation of 
three of the first eight positive integers is given by (5,7,2)

The statistic P(n,k) counts the total number of partial permutations of k 
objects that can be formed from a collection of n objects. Note that P(n,n)
is just the number of permutations of n objects, which we found to be equal to 

n!=n(n−1)(n−2)...(3)(2)

in “Enumerating Gene Orders”.
--}

partial_permutation :: Integer -> Integer -> Rational
partial_permutation n k = toRational (product [(n - pred k) .. n])

choose :: Integer -> Integer -> Rational
choose n k = partial_permutation n k / factorial k

{-- RECURRENT RELATIONS --------------------------------------------------
First of all, recall that recur was defined as follows:
recur :: Integer -> Integer -> Integer
recur 1 k = 1
recur 2 k = 1
recur n k = recur (pred (pred n)) k * k + recur (pred n) k
but recall that dynamic programming allows us to speed up this computation:
--}

recur :: Integer -> Integer -> Integer
recur = recurring [1,1]

recurring :: [Integer] -> Integer -> Integer -> Integer
recurring (a:b:_) n k | n < 1     = 0
                      | n < 3     = a
                      | otherwise = recurring [b * k + a,a] (pred n) k

{--
>>> recur 5 3
19
>>> recur 35 2 
11453246123
...in no time
Note that I drop 'older' elements of the recurrence list: after they are 
computed, they are no longer needed and discarded, so the recurrence 
relationship is now efficient in both time and space, whereas the original
doubly recursive algorithm is efficient in neither.
--}

{-- FIBONACCI ------------------------------------------------------------
Recall the doubly-recursive definition of fibonacci is:

fibo :: Integer -> Integer
fibo n | n <  1    = 0
       | n == 1    = 1
       | n >  1    = fibo (n-1) + fibo (n-2)

But what is the value of fibo really?

That's a problem, isn't it, because fibo is in exponential time if defined
doubly-recursively. But here's the thing: if you know F(n) you already know
F(n-1) ... so why recompute that subtree when you've already just computed it
So, use that knowledge. Retain it. Define a fibonacci function that returns
the fibonacci at n in linear time by retaining the previous [0..n-1] fibonacci
numbers. This is what we call dynamic programming.

fibr :: [Integer] -> Integer -> Integer
fibr _ 0 = 0
fibr (x:_) 1 = x
fibr fibs@(x:y:_) n = fibr (x+y:fibs) (pred n)

-- of course you need to seed your fibonacci computer for it to work. What shall
-- your seed be?

seed :: [Integer]
seed = [1,0]

-- What is the values of map (fibr seed) [6, 25, 100]? 
-- Are they return timely?

>>> map (fibr seed) [6, 25, 100]
[8,75025,354224848179261915075]
returned with no delay.

Epilogue.

Fibonacci is a specific expression of a recurrence relation.

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
