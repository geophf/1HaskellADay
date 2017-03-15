module Analytics.Theory.Number.Prime where

-- from O'Neill's algorithm via
-- http://www.garrisonjensen.com/2015/05/13/haskell-programs-are-lies.html

import qualified Data.Set as PQ

-- below import available via 1HaskellADay git repository

import Analytics.Theory.Number.SquareRoot

type Prime = Integer

primes :: [Prime]
primes = 2:sieve [3,5..]
  where
    sieve (x:xs) = x : sieve' xs (insertprime x xs PQ.empty)

    sieve' (x:xs) table
        | nextComposite == x = sieve' xs (adjust x table)
        | otherwise          = x : sieve' xs (insertprime x xs table)
      where 
        (nextComposite,_) = PQ.findMin table

    adjust x table
        | n == x    = adjust x (PQ.insert (n', ns) newPQ)
        | otherwise = table
      where
        Just ((n, n':ns), newPQ) = PQ.minView table

    insertprime p xs = PQ.insert (p*p, map (*p) xs)

-- Shows a number is prime up to sqrt n or is not prime because factor f

prime :: Integer -> [Prime] -> Either Prime Integer
prime x = prime' x (iSqrt x 1)

prime' :: Integer -> Integer -> [Prime] -> Either Prime Integer
prime' x y (z:rest) | x `mod` z == 0 = Right z   -- factor z: not prime
                    | y < z          = Left y    -- prime up to y
                    | otherwise      = prime' x y rest

-- then the test for primality falls right out of the above

isPrime :: Integer -> Bool
isPrime = either (const True) (const False) . (`prime` primes)
