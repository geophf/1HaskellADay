module Y2016.M12.D12.Solution where

import Control.Monad (guard)

{--
Today's Haskell exercise is from the Mensa Genuis Quiz-a-Day book. And I quote:

"Aha," said the math professor to her husband, "it's a very simple problem, but
I see that our two sons have now reached interesting ages. The product of their
ages is six times the amount you get if you add their ages. On the other hand,
if you add the square of each age, the total of the two will be 325." How old
were their boys?

Okay, solve that. ... How?

Question: do you have conversations like this around the dinner table, ...
like I do?

Solution:

One way to look at this problem is that it is a 'spreadsheet problem.' 

1. What are all the integral values of y given 325-x*x?

(first of all, we know, therefore, that x*x is less than 325, so what is the
range of x?)
--}

rangex :: [Int]
rangex = takeWhile (\x -> x * x <= 325) [1..]

{-- we also assume x is born, so that's good.
*Y2016.M12.D12.Solution> rangex ~> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]

Okay, so now we have x nailed, we also assume, weirdly enough, that x and y
are integral ... this presupposes that x and y are born on the same day of the
year, but we will accept that and leave aside the inconsistency of the problem
statement, because the problem was written in this imprecise language, English,
and not in 'Math', so we must forgive it this laxity.

So, then, what are the possible integral values of x and y, given that

x*x + y*y = 325

Well, we know the range of x. Let's use that as the starting point.
--}

possibleIntegralAges :: [(Int, Int)]
possibleIntegralAges = map fromIntegral rangex >>= \x ->
   let y = sqrt (325 - x*x) in
   guard (floor (y * 1000) == floor y * 1000) >> -- imprecise, but close enough
   return (floor x, floor y)

-- *Y2016.M12.D12.Solution> possibleIntegralAges ~> [(1,18),(6,17),(10,15),(15,10),(17,6),(18,1)]

-- These are the values we work with the first part of the equation.

productSum :: [(Int, Int)] -> [(Int, Int)]
productSum = filter (\(x,y) -> x*y == 6 * (x + y))

twoBirthdayBoyz :: [(Int, Int)]
twoBirthdayBoyz = productSum possibleIntegralAges

-- *Y2016.M12.D12.Solution> twoBirthdayBoyz ~> [(10,15),(15,10)]

-- Their boys are 15 and 10 years old.


