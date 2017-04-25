module Y2017.M04.D25.Solution where

import Control.Monad (guard)

-- below import available via 1HaskellADay git repository

import Data.Numeral.QBits
import Data.QBit

{--
From the Mensa Genuius Quiz-a-Day Book by Dr. Abbie F. Salny.

One can replace the letters with numbers so the addition will be correct
numerically. (Hint K = 9)

      M O M
      M O M
  +     N O
  ---------
    B O O K

hint-hint: You've looked at solving this kind of problem before (see above)
--}

momMomNoBook :: [Digit] -> [(Int, Int, Int)]
momMomNoBook mom@[m,o,n,b,k] =
   draws mom pool >>= \([m1,o2,n6,b4,k5], _) ->
   let [n1, n2, n3] = map (asNum . N) [[m1,o2,m1],[n6,o2],[b4,o2,o2,k5]] in
   guard (2 * n1 + n2 == n3)      >>
   return (n1, n2, n3)

-- sum [mom, mom, no] == book ... simple, eh?

-- Also recall that M, N, B cannot be 0 and given K == 9, O has a certain property

{-- 
Okay, but how do we query momMomNoBook with the proper constraints, and what
are the proper constraints?

Constraints:

1. k = 9
2. m, n, b > 0
3. We know from SCIENCE that B < 3 (why? prove it)
4. We know from SCIENCE that o is odd (why? prove it)

So, is 0 in the draw-pool? No. M > 0, O is odd, N > 0, B > 0 and K == 9:
There is no zero to consider. And since we know k == 9, we don't need to
consider that as a possibility, either.
--}

pool :: [Int]
pool = [1..8]

{--
So:
>>> momMomNoBook [constrain notZero, constrain isodd, constrain notZero, constrain notZero, Observed 9]
[(757,45,1559)]

gives the result in a blink.
--}
