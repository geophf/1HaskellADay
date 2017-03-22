{-# LANGUAGE ViewPatterns, TupleSections #-}

module Y2017.M03.D22.Solution where

import Data.Maybe

-- below imports available via 1HaskellADay git repository

import Analytics.Theory.Number.Prime
import Control.DList

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

-- first we need the primes up to the number ... in two lists:

primesUpto :: Int -> ([Prime], [Prime])
primesUpto = upto primes [] emptyDL . fromIntegral

upto :: [Prime] -> [Prime] -> DList Prime -> Integer -> ([Prime], [Prime])
upto (h:t) accum dlist n | h >= n = (dlToList dlist, accum) 
                         | otherwise = upto t (h:accum) (dlist <| h) n

goldbach :: Int -> [(Prime, Prime)]
goldbach = gb . primesUpto <*> fromIntegral

gb :: ([Prime], [Prime]) -> Integer -> [(Prime, Prime)]
gb ((p1:ps1), (p2:ps2)) n | n `mod` 2 == 1 = []
                          | p1 + p1 > n    = []
                          | p1 + p1 == n   = [(p1, p1)]
                          | p1 + p2 >  n   = gb ((p1:ps1), ps2) n
                          | p1 + p2 == n   = (p1, p2) : gb (ps1, ps2) n
                          | p1 + p2 <  n   = gb (ps1, (p2:ps2)) n

{--
First make sure we are proving Goldbach's conjecture for an EVEN number.
How do you do that? Because if I give you the number 23, all bets are off,
aren't they? Make that so, Number One!
--}

-- Prove Goldbach's Conjecture for the following numbers. If a number is not
-- even, fail out.

sample :: [Int]
sample = [5, 12, 23, 52, 99]

heador :: [a] -> Maybe a
heador [] = Nothing
heador (h:_) = Just h

proof :: Int -> Maybe Proof
proof n = (From (fromIntegral n)) <$> heador (goldbach n)

data Proof = From Integer (Prime, Prime)

instance Show Proof where
   show (From n p) = show n ++ " |- " ++ show p

{--
>>> mapM_ print (mapMaybe proof sample)
12 |- (5,7)
52 |- (5,47)
--}

-- How about something a bit more beefy?
-- Prove Goldbach's Conjecture for the following.

beefy :: [Int]
beefy = [123456, 626, 314159, 9998]

{--
>>> mapM_ print (mapMaybe proof beefy)
123456 |- (7,123449)
626 |- (7,619)
9998 |- (31,9967)
--}

-- There, we've definitively proved Goldbach's Conjecture! See you at the pub!
