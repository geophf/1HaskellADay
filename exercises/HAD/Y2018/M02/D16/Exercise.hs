module Y2018.M02.D16.Exercise where

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

coprime :: Integral a => a -> a -> Bool
coprime a b = undefined

{--
>>> coprime 35 64
True
--}

-- Determine if these pairs of Ints are coprime
-- numbers provided by random.org

jack0diamonds :: [(Int, Int)]
jack0diamonds = [(485319,761353), (991054, 636321), (503867, 517005)]

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
pairs file = undefined

-- create a report of the coprime pairs from rndnum.txt in this format:

-- [((328286,30706),False),((429273,188961),False),((517634,176001),True),...]
