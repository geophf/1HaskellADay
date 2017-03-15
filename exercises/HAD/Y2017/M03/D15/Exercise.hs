module Y2017.M03.D15.Exercise where

import Data.Array
import Data.Set (Set)

-- below import available via 1HaskellADay git repository

import Analytics.Math.Combinatorics (choose)

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

squareFree :: Integer -> Bool
squareFree n = undefined

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

pascalRow' :: Integer -> PascalRow
pascalRow' row = undefined

-- of course the pascalRow' n has n columns

-- Show rows 1 - 8 of Pascal's Triangle as computed by pascalRow'

-- What is the value of pascalRow' 51? How long did it take?

{--
Of course, there are multiple inefficiencies here, as we compute each value of
a pascal row out of context. So what is a context that we can use? Well we know
that for the nth pascal row (pr) that the mth column is:

1 for m == 1 or n; and
prevrow ! (pred m) + prevrow ! m otherwise

so, knowing this, write prcol given prevrow (where the first PascalRow is [1]
--}

prcol :: PascalRow -> Int -> Int -> Integer
prcol prevRow n m = undefined

-- Given prcol, write pascalRow:

pascalRow :: PascalRow -> Int -> PascalRow
pascalRow prevRow n = undefined

firstRow :: PascalRow
firstRow = array (1,1) [(1,1)]

-- Again, what is the 51st row of the Pascal triangle?

-- What are the unique values of the first 51 rows of the Pascal triangle?
-- You should have (significantly fewer) values than a 5565 count. How many
-- unique values do you have? What is the greatest value that you have?

uniqueValuesUpTo :: Int -> Set Integer
uniqueValuesUpTo row = undefined

-- Tomorrow we may or may not look at factorizing these puppehz! It may involve
-- Elliptic curve factorization or it may involve more direct (brutish) prime
-- factorization. We shall see. Your thoughts?
