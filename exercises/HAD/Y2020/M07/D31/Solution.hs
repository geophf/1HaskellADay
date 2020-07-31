module Y2020.M07.D31.Solution where

{--
Well, that's random.

Random numbers, we are told, are a hard thing. I mean, I just can't think of
the random number: 42, and get all sorts of vicious backblow about how that's
not random because of Douglas Adams.

So, we're going to tackle random numbers today.

Today, ...

... no, yesterday! we had a list of active team members. If you were to order
them from 1 to n, because this is Prolog, all of the sudden, then, first:

a) generate some random between 1 and n.
--}

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random

rnd :: Int -> IO Int
rnd hi = randomIO >>= return . succ . flip mod hi

{--
>>> rnd 9
9
>>> rnd 9
3
>>> rnd 9
7

b) Now, return 1 .. n in a random order, so:

>>> rnds 9
[8,2,7,5,1,9,3,4,6]
--}

rnds :: Int -> IO [Int]

{--
rnds hi = mapM rnd (take hi (cycle [hi]))

>>> rnds 9
[8,6,7,4,4,8,1,8,4]

Nupe, because we have duplicates. How do we get rid of duplicates? Lots of
different ways to do this. Let's try one.
--}

rnds = rnds' Set.empty 0 []

rnds' :: Set Int -> Int -> [Int] -> Int -> IO [Int]
rnds' set len accum hi | len == hi = return accum
                       | otherwise = rnd hi >>= rnds'' set len accum hi

rnds'' :: Set Int -> Int -> [Int] -> Int -> Int -> IO [Int]
rnds'' set len accum hi val | Set.member val set = rnds' set len accum hi
                            | otherwise = 
   rnds' (Set.insert val set) (succ len) (val:accum) hi

{--
>>> rnds 9
[3,6,1,5,4,7,2,9,8]

>>> rnds 9
[5,6,4,3,1,8,7,9,2]

>>> rnds 9
[7,6,4,3,5,8,1,2,9]

So, ... yeah.
--}
