module Y2017.M03.D22.Exercise where

import Analytics.Theory.Number.Prime

{--
Okay, we WERE going to go onto a probability study, and we ARE, but first, let's
take a little detour and prove at Goldbach's conjecture.

Goldbach's conjecture poses that every even number greater than 2 is the sum
of two primes.

No one has ever proved it for all even numbers.

Until today.

Today's Haskell problem. Prove that every even number in the set of all even
numbers (given) are the sum of two prime numbers

Well, we don't have to prove this for ALL even numbers, because that's 
ridiculous! Let's just take a sample here that's "good enough," then call it
a day and hit the pub, awaiting our accolades to come.
--}

goldbach :: Int -> [(Prime, Prime)]
goldbach num = undefined

{--
First make sure we are proving Goldbach's conjecture for an EVEN number.
How do you do that? Because if I give you the number 23, all bets are off,
aren't they? Make that so, Number One!
--}

-- Prove Goldbach's Conjecture for the following numbers. If a number is not
-- even, fail out.

sample :: [Int]
sample = [5, 12, 23, 52, 99]

-- How about something a bit more beefy?
-- Prove Goldbach's Conjecture for the following.

beefy :: [Int]
beefy = [123456, 626, 314159, 9998]

-- There, we've definitively proved Goldbach's Conjecture! See you at the pub!
