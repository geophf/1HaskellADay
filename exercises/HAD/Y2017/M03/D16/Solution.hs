module Y2017.M03.D16.Solution where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available from 1HaskellADay git respository

import Analytics.Theory.Number.Prime

import Y2017.M03.D15.Solution (uniqueValuesUpTo)

{--
So, yesterday, when we solved the Exercise imported above, we saw that we had
614 unique values with the max value being Just 126410606437752. That max value
is quite the spicy meatball, however. But a help here is that we are looking
for prime-square-free numbers, or, that is to say more precisely, numbers that
are not divisible by the squares of the primes. So, let's winnow down our list
a bit.
--}

squareFreed :: Prime -> Set Integer -> Set Integer
squareFreed prime = Set.filter ((/= 0) . (`mod` (prime ^ 2)))

{--
How many values in uniqueValuesUpTo 51 when that list is squareFreed of the
first Prime, 2? (remember to square 2 as the factor to check against)o

>>> length (squareFreed (uniqueValuesUpTo 51) (head primes))
286

>>> fst <$> Set.maxView (squareFreed (head primes) (uniqueValuesUpTo 51))
Just 18053528883775

Boom! A marked improvement! Let's do the same for the next prime, 3. First,
assign smr0 to the squareFreed 2 value:

>>> let smr0 = squareFreed (head primes) (uniqueValuesUpTo 51)

and repeat the above for the next prime (head (tail primes)). What is the new
length and new max value you get for your newly filtered set from smr0?

assign smr1 to that newer smaller subset. 

>>> let smr1 = squareFreed (head (tail primes)) smr0

>>> length smr1
185

>>> fst <$> Set.maxView smr1
Just 18053528883775

No change to the max, let's go a bit further. Now how about for the next prime?

>>> let smr2 = squareFreed (head (drop 2 primes)) smr1

>>> length smr2
162

>>> fst <$> Set.maxView smr2
Just 9762479679106

>>> sqrt . fromIntegral <$> it
Just 3124496.708128527

This shows after filtering out only 3 prime-squares we've significantly reduced
the number of values we need to test against AND the maximum value prime we need
to compute to test.

So.

Today's Haskell problem. Using the above algorithm, filter the unique values
of the Pascal's Triangle up to row 51 down to only the square-free numbers,
then sum the resulting set. What is the value you get?
--}

sqFreeSieve :: Set Integer -> Set Integer
sqFreeSieve = sfs primes

sfs :: [Prime] -> Set Integer -> Set Integer
sfs (p:rimes) uniq =
   if fromMaybe 0 (fst <$> Set.maxView uniq) < p * p then uniq
   else sfs rimes (squareFreed p uniq)

{--
>>> length (sqFreeSieve (uniqueValuesUpTo 51))
158

Eheh! We found only four more beasties for all that work?!? ;)

>>> sum (sqFreeSieve (uniqueValuesUpTo 51))

... some value
--}
