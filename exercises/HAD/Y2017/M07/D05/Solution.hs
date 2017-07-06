module Y2017.M07.D05.Solution where

import Control.Monad (guard)

-- below imports available via 1HaskellADay git repository

import Data.QBit
import Data.Numeral.QBits

{--
AHA! I'm BACK!
I'm BACK in the saddle again!

From the Mensa Genius Quiz-a-Day Book by Dr. Abbie F. Salny, July 3 problem:

The following multiplication example uses all the digits from 0 to 9 once and
only once (not counting the intermediate steps). Finish the problem. One number
has been filled in to get you started.

          x x x     (a)
            x 5     (b)
      ---------
      x x x x x     (c)

DO IT TO IT!
I LIKE TO MOVE IT, MOVE IT!
--}

{-- 
Now we need properly structure our input values.

We know that the last digit of c MUST be zero (it can't be 5), so we know
the last digit of a MUST be even AND NOT zero.

Also we know the first digit of each num MUST be greater than zero.

Thus we have:
--}

abc :: (Nums, Nums, Nums)
abc = (N [constrain notZero, free, constrain ((&&) . notZero <*> iseven)],
       N [constrain notZero, Observed 5],
       N [constrain notZero, free, free, free, Observed 0])

-- given the above foreknowledge, we assign the known/observed values first:

multiplicationProblem :: Nums -> Nums -> Nums -> [(Nums, Nums, Nums)]
multiplicationProblem (N a) (N b) (N c) =
   draws b [0..9] >>= \(b', l1) -> let n2 = N b' in
   draws c l1     >>= \(c', l2) -> let n3 = N c' in
   draws a l2     >>= \(a', l3) -> let n1 = N a' in
   guard (null l3)                         >>
   guard (asNum n1 * asNum n2 == asNum n3) >>
   return (n1, n2, n3)

{--
>>> (a,b,c) = abc
>>> multiplicationProblem a b c
[(396,45,17820)]
--}
