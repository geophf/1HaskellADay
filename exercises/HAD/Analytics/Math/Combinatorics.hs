module Analytics.Math.Combinatorics where

-- Where you want to get all factorial and stuff

factorial :: Integer -> Rational
factorial = toRational . product . enumFromTo 1

choose :: Integer -> Integer -> Rational
choose n k = factorial n / (factorial k * factorial (n - k))

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
--}

fibr :: [Integer] -> Integer -> Integer
fibr _ 0 = 0
fibr (x:_) 1 = x
fibr fibs@(x:y:_) n = fibr (x+y:fibs) (pred n)

-- of course you need to seed your fibonacci computer for it to work. What shall
-- your seed be?

seed :: [Integer]
seed = [1,0]

-- What is the values of map (fibr seed) [sample, big, really]? 
-- Are they return timely?

{--
>>> map (fibr seed) [6, 25, 100]
[8,75025,354224848179261915075]
returned with no delay.

Epilogue.

There is no law stating you couldn't've defined fibo in terms of fibr from the
get-go, knowing that binary functions can be linearized (Paul Taurau's 
binary prolog papers come to mind here). So:
--}

fibonacci :: Integer -> Integer
fibonacci = fibr seed

-- is a perfectly acceptable definition

-- There are two problems with fibr which we will address, and then there is
-- a more general recurrence relationship by which we may define fibonacci
