module Y2018.M02.D27.Exercise where

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

data Peeps = JT | Laura | Aditya
   deriving (Eq, Show)

type Day = Int

scheduler :: [(Peeps, Day)] -> [[Peeps]]
scheduler schedulesFor = undefined

-- with the n'th value with all three being how often.

-- or you could solve it mathematically to:

oftenness :: [(Peeps, Day)] -> Day
oftenness schedulesFor = undefined

-- with the n'th day being returned MATHEMATICALLY!

-- your choice.
