module Y2017.M03.D14.Solution where

import Data.Char (ord)
import Data.Ratio (numerator)

{--
So, today, 3 . 14 is π-day (March 14th).

One of the projecteuler.net problems, problem 65 is a problem along the way of
solving for the "As Easy As Pi" award, so:

Convergents of e
problem 65

url: https://projecteuler.net/problem=65

The square root of 2 can be written as an infinite continued fraction:


√2 = 1 + (1 / (2 + 1 / (2 + 1 / (2 + 1 / (2 + ...

The infinite continued fraction can be written √2 = [1;(2)], (2) indicates that
2 repeats ad infinitum. In a similar way, √23 = [4; (1,2,1,8)].

It turns out that the sequence of partial values of continued fractions for 
square roots provide the best rational approximations. Let us consider the 
convergents for √2.

1 + 1/2 = 3/2

1 + 1/(2 + 1/2) = 7/5

1 + 1/(2 + 1/(2 + 1/2)) = 17/12

1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29

Hence the sequence of the first ten convergents for √2 are:

1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...

What is most surprising is that the important mathematical constant,

e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].

The first ten terms in the sequence of convergents for e are:

2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...

The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.

Find the sum of digits in the numerator of the 100th convergent of the continued
fraction for e.
--}

type ContinuedFraction = (Int, [Int])

nthConvergent :: Int -> ContinuedFraction -> Rational
nthConvergent n (w, rest) =
   fromIntegral w + continue (pred n) (map fromIntegral rest)

continue :: Int -> [Rational] -> Rational
continue 0 _ = 0
continue n (h:t) = 1 / (h + continue (pred n) t)

-- given sqrt2 as a continued fraction:

sqrt2 :: ContinuedFraction
sqrt2 = (1, repeat 2)

{--
Show the first 5 partial values for √2

>>> nthConvergent 1 sqrt2
1 % 1
>>> nthConvergent 2 sqrt2
3 % 2
>>> nthConvergent 3 sqrt2
7 % 5
>>> nthConvergent 4 sqrt2
17 % 12
>>> nthConvergent 5 sqrt2
41 % 29
--}

-- define e as a continued fraction:

e :: ContinuedFraction
e = (2, concatMap evens [1..])
   where evens n = [1, 2 * n, 1]

{--
Show the 10th convergent of e

>>> nthConvergent 10 e
1457 % 536
--}

sumNumerator :: Rational -> Int
sumNumerator = sum . map digit . show . numerator
   where digit d = ord d - ord '0'

{--
What is the sum of the digits of the numerator of the 10th convergent of e?

>>> sumNumerator (nthConvergent 10 e)
17

What is the value of sumNumerator (nthConvergent 100 e)?

sumNumerator (nthConvergent 10 e)

is ... something.

Moving this solution to Analytics.Math.ContinuedFractions
--}
