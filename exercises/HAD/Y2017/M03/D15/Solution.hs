module Y2017.M03.D15.Solution where

import Data.Array (Array, listArray, (!), elems)
import Data.Ratio (numerator)
import Data.Set (Set)
import qualified Data.Set as Set

-- below import available via 1HaskellADay git repository

import Analytics.Theory.Number.Combinatorics (choose)

{--
Today, in the U.S.A. Corporate tax forms are due to the IRS. I mention this in 
passing.

Let's look at a projecteuler.net problem a little bit further down the line:

Squarefree Binomial Coefficients
Problem 203

url: https://projecteuler.net/problem=203

The binomial coefficients n choose k can be arranged in triangular form, 
Pascal's triangle, like this:

                      1	
                    1   1	
                  1   2   1
                1   3   3   1
              1   4   6   4   1
            1   5  10  10   5   1
          1   6  15  20  15   6   1
        1   7  21  35  35  21   7   1
                 .........

It can be seen that the first eight rows of Pascal's triangle contain twelve 
distinct numbers: 1, 2, 3, 4, 5, 6, 7, 10, 15, 20, 21 and 35.

A positive integer n is called squarefree if no square of a prime divides n. Of 
the twelve distinct numbers in the first eight rows of Pascal's triangle, all 
except 4 and 20 are squarefree. The sum of the distinct squarefree numbers in 
the first eight rows is 105.

Find the sum of the distinct squarefree numbers in the first 51 rows of Pascal's
triangle.
--}

type PascalRow = Array Int Integer

{--
Huh.

We can see that these numbers can get big:

>>> 51 `choose` 25
247959266474052 % 1

But the good news is that there are only a few of these numbers:

>>> sum [1 .. 51]
1326

And uniqueness wipes out the redundant ones, which removes a majority of them.

So, first up, what are the first 51 rows of the Pascal's triangle?

One way to compute each row is: row `choose` column.

Try it this way:
--}

pascalRow' :: Int -> PascalRow
pascalRow' row = let r = pred row in listArray (1, row) 
       $ map (numerator . (fromIntegral r `choose`) . fromIntegral) [0 .. r]

-- of course the pascalRow n has n columns

{-- Show rows [1 .. 8] of Pascal's Triangle

>>> mapM_ (print . elems) (map pascalRow' [1 .. 8])
[1]
[1,1]
[1,2,1]
[1,3,3,1]
[1,4,6,4,1]
[1,5,10,10,5,1]
[1,6,15,20,15,6,1]
[1,7,21,35,35,21,7,1]

What is the value of pascalRow' 51? How long did it take?

>>> elems $ pascalRow' 51
[1,50,1225,19600,230300,2118760,15890700,99884400,536878650,2505433700,
 10272278170,37353738800,121399651100,354860518600,937845656300,2250829575120,
 4923689695575,9847379391150,18053528883775,30405943383200,47129212243960,
 67327446062800,88749815264600,108043253365600,121548660036300,126410606437752,
 121548660036300,108043253365600,88749815264600,67327446062800,47129212243960,
 30405943383200,18053528883775,9847379391150,4923689695575,2250829575120,
 937845656300,354860518600,121399651100,37353738800,10272278170,2505433700,
 536878650,99884400,15890700,2118760,230300,19600,1225,50,1]

Of course, there are multiple inefficiencies here, as we compute each value of
a pascal row out of context. So what is a context that we can use? Well we know
that for the nth pascal row (pr) that the mth column is:

1                                     for m == 1 or n; and
prevrow ! (pred m) + prevrow ! m      otherwise

so, knowing this, write prcol given prevrow (where the first PascalRow is [1]
--}

prcol :: PascalRow -> Int -> Int -> Integer
prcol prev n 1 = 1
prcol prev n m | n == m    = 1
               | otherwise = prev ! (pred m) + prev ! m

-- Given prcol, write pascalRow:

pascalRow :: PascalRow -> Int -> PascalRow
pascalRow prevRow n = listArray (1,n) (map (prcol prevRow n) [1 .. n])

firstRow :: PascalRow
firstRow = listArray (1,1) [1]

{-- 
Again, what is the 51st row of the Pascal triangle?

>>> foldl pascalRow firstRow [1 .. 51] == pascalRow' 51
True
--}

uniqueValuesUpTo :: Int -> Set Integer
uniqueValuesUpTo row = foldr (\r s -> Set.union s . Set.fromList . elems $ pascalRow' r) Set.empty [1 .. row]

{--
What are the unique values of the first 8 rows of the Pascal triangle?

>>> uniqueValuesUpTo 8 == Set.fromList [1,2,3,4,5,6,7,10,15,20,21,35]
True

* What are the unique values of the first 51 rows of the Pascal triangle?
You should have (significantly fewer) values than a 1326 count. 

>>> uniqueValuesUpTo 51
fromList [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
  27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,55,56,
  66,70,78,84,91,105,120,126,136,153,165,171,190,210,220,231,252,253,276,286,
  300,325,330,351,364,378,406,435,455,462,465,495,496,528,560,561,595,630,666,
  680,703,715,741,780,792,816,820,861,903,924,946,969,990,1001,1035,1081,1128,
  1140,1176,1225,1287,1330,1365,1540,1716,1771,1820,2002,2024,2300,2380,2600,
  2925,3003,3060,3276,3432,3654,3876,4060,4368,4495,4845,4960,5005,5456,5984,...]

* How many unique values do you have? 

>>> length (uniqueValuesUpTo 51)
614

* What is the greatest value that you have?

>>> fst <$> Set.maxView (uniqueValuesUpTo 51)
Just 126410606437752

Tomorrow we may or may not look at factorizing these puppehz! It may involve
Elliptic curve factorization or it may involve more direct (brutish) prime
factorization. We shall see. Your thoughts?
--}
