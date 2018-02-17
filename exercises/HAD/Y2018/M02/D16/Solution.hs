module Y2018.M02.D16.Solution where

{--
I know what you're thinking.

(I'm all psycho like that)

You're thinking: "Hahaha, geophf! You published an exercise yesterday that had
a solution of:

gcd = Prelude.gcd

Hahaha!"

Well, okay, ... sue me, sue me, shoot bullets through me, ... I love you.

Sigh. Does anybody sing it any better than Frank?

Well, I did that exercise yesterday, because 1. I wanted to, and 2. I have
today's exercise which is the very next exercise from P99, archived at:

http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

(*) Determine whether two positive integer numbers are coprime.
Two numbers are coprime if their greatest common divisor equals 1.
Example:
?- coprime(35, 64).
Yes
--}

import Control.Arrow ((&&&))

coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1

{--
>>> coprime 35 64
True
--}

-- Determine if these pairs of Ints are coprime
-- numbers provided by random.org

jack0diamonds :: [(Int, Int)]
jack0diamonds = [(485319,761353), (991054, 636321), (503867, 517005)]

{--
>>> map (uncurry coprime) jack0diamonds 
[True,True,True]

Well, I'll be blowed.
--}

{-- BONUS -----------------------------------------------------------------

From the set of numbers stored in:
--}

rndNums :: FilePath
rndNums = "Y2018/M02/D16/rndnums.txt"

-- How many coprime pairs are there of adjacent pairs?
-- Did you have to consider efficiency in your search, or nah?

{--
>>> nums <- concatMap words . drop 2 . lines <$> readFile rndNums 
>>> length nums
94

pairwise :: [a] -> [(a,a)]
pairwise [_] = []
pairwise (a:b:rest) = (a,b) : pairwise (a:rest) ++ pairwise (b:rest)

Hm.

>>> product [1..94]
108736615665674308027365285256786601004186803580182872307497374434045199869417927630229109214583415458560865651202385340530688000000000000000000000

... well, I don't have all day, so ...
--}

pairOff :: [a] -> [(a,a)]
pairOff (a:b:rest) = (a,b) : pairOff rest
pairOff [_] = []
pairOff [] = []

pairs :: FilePath -> IO [(Int, Int)]
pairs = fmap (pairOff . concatMap (map read . words) . drop 2 . lines) . readFile

{--
>>> nums <- pairs rndNums 
>>> length nums
47
>>> map (id &&& uncurry coprime) nums
[((328286,30706),False),((429273,188961),False),((517634,176001),True),
 ((677477,653888),True),((246741,420661),True),((66629,709915),True),
 ((799151,997273),True),((572934,808556),False),((616540,207749),True),
 ((649519,384662),True),((836517,307052),True),((274516,464386),False),
 ((804680,557979),True),((420563,661838),True),((563989,20483),True),
 ((24455,614426),True),((938883,95120),True),((360879,621789),False),...]
--}
