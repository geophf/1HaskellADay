{-# LANGUAGE ViewPatterns #-}

module Y2017.M03.D23.Solution where

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)

{--
Okay, we're going explore a bit of probability and see where that takes us.

These problems are from Fifty Challenging Problems in Probability with Solutions
by Frederick Mosteller.

Problem 1

A drawer contains red socks and black socks. When two socks are drawn at random,
the probability that both are red is 1/2.
--}

-- (a) How small can the number of socks in the drawer be?

{-- 
Mosteller describes solving this problem in this manner. From general
probability theory we know 

P(RR) = (r / (r + b)) * pred r / (pred r * b) 

as drawing a sock is independent of drawing another sock. And our aim is:

P(RR) = 1/2

So this means

(r / (r + b)) ^ 2 > 1/2 > (pred r / (pred r * b)) ^ 2

So we solve for and minimize integral r and b that satisfy the above inequalities.

Taking the sq roots:

r / (r + b) > 1 / sqrt 2 > pred r / (pred r + b)

For the left inequality, solving for r we get:

r > (1 + sqrt 2) * b

And on the right:

(1 + sqrt 2) * b + 1 > r

So r falls between these two values
--}

rGiven :: Int -> [Int]
rGiven (fromIntegral -> b) =
   let m = 1 + sqrt 2
       lowerb = ceiling (m * b)
       upperb = floor (m * b) + 1
   in  [lowerb .. upperb]

pRR :: (Int, Int) -> Rational
pRR (adjoin fromIntegral -> (r,b)) = (r / (r + b)) * (r - 1) / (r + b - 1)

rb :: [(Int, Int)]
rb = filter ((== 1/2) . pRR) ([1..] >>= \b -> rGiven b >>= \r -> return (r, b))

totalSocks :: Int
totalSocks = uncurry (+) $ head rb

{--
>>> totalSocks
4

The kicker here is, we do not, as reason dictates, have 1 pair of black socks
and 1 pair of red socks. No: we have 3 red socks and 1 black sock in our sock
drawer.

Okay, professor, you've been hanging onto that one black sock for HOW long?

And how much was spent educating you in your specialization? But okay.
--}

-- (b) How small if the number of black socks is even?

{--
This is a further constraint, but not an onerous one. We simply solve for all
solutions, the first one (the minimum) solving the first problem, and then
filter on the head of all even b's
--}

totalSocksGivenEvenBlack :: Int
totalSocksGivenEvenBlack = uncurry (+) . head $ filter (even . snd) rb
   where even = ((== 0) . (`mod` 2))

{--
>>> totalSocksGivenEvenBlack 
21

This is even worse that the professor who does not sort his socks, because in
this case we have 15 red socks (leaving an unmached pair). And this is a Good
Thing (TM)?

There was a projecteuler.net problem asking this problem for a very large 
total number of socks. Does anyone recall which problem it was?
--}

{--
Okay, serious question to you red-or-black sock owners.

(1) Who has a drawer full of RED SOCKS? Is this some College professor-thing?

(2) Who, after a week, hasn't figured out you put the RED SOCKS on the LEFT side
    of the drawer and BLACK SOCKS on the RIGHT side of the drawer, so your 
    probability of drawing two red socks is 100% because you're not a dummy?

    It's called algebraic types, and it's good for more than just Haskell.
    Just saying.

    But no, that's okay: arrange your sock drawer in whichever way your heart 
    desires.
--}
