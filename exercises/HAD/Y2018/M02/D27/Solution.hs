module Y2018.M02.D27.Solution where

{--
From 

https://www.scientificamerican.com/article/can-you-solve-these-mensa-puzzles5/

(7) If JT works one shift every second day, Laura works once every third day,
    and Aditya works every fifth day, how often do all three colleagues work
    together?

This problem looks very fizz-buzzy.

HOW DO YOU SOLVE THIS?

Here's one approach:
--}

import Data.Tuple (swap)

-- below import available via 1HaskellADay git repository

data Peeps = JT | Laura | Aditya
   deriving (Eq, Show)

type Day = Int

scheduler :: [(Peeps, Day)] -> [[Peeps]]
scheduler = sched' 1

-- with the n'th value with all three being how often.

-- sched' generates an infinite list, because "... the work is never done."

sched' :: Int -> [(Peeps, Day)] -> [[Peeps]]
sched' x = (:) . map fst . filter ((== 0) . mod x . snd) <*> sched' (succ x)

{-- is this a scan function? It ... kinda ... looks like a scan function.

Anyway.

>>> take 30 (scheduler [(JT,2),(Laura,3),(Aditya,5)])
[[],[JT],[Laura],[JT],[Aditya],[JT,Laura],[],[JT],[Laura],[JT,Aditya],[],
 [JT,Laura],[],[JT],[Laura,Aditya],[JT],[],[JT,Laura],[],[JT,Aditya],[Laura],
 [JT],[],[JT,Laura],[Aditya],[JT],[Laura],[JT],[],[JT,Laura,Aditya]]

So, every month-...ish.
--}

-- or you could solve it mathematically, too:

oftenness :: [(Peeps, Day)] -> Day
oftenness = -- of course, this is the least common multiple
   foldr lcm 1 . map snd

-- with the n'th day being returned MATHEMATICALLY!

{--
>>> oftenness [(JT,2),(Laura,3),(Aditya,5)]
30

Does the lcm function commute? Prove that it does or doesn't.
--}
