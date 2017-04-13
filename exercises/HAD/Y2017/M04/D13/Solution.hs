module Y2017.M04.D13.Solution where

import Control.Monad (guard)

-- below import available via 1HaskellADay git repository

import Data.QBit

{--
Another puzzler from the Mensa Genius Quiz-a-Day Book by Dr. Abbie Salny.

The following multiplication problem uses every digit from 0 to 9 exactly once

     7 x x
       4 x
 ---------
 x x x x x

Fill in the missing numbers.
--}

type Digit = QBit Int

data Nums = N [Digit]

instance Show Nums where
   show (N digs) = concatMap show digs

asNum :: Nums -> Int
asNum (N digs) = n 0 (reverse digs)
   where n _ [] = 0
         n p (h:t) = 10 ^ p * extract h + n (succ p) t

-- so, with all t he the above, it makes sense to work with Nums values

multiplicationPuzzle :: Nums -> Nums -> Nums -> [(Int, Int, Int)]
multiplicationPuzzle (N x) (N y) (N z) =
   draws x [0..9]           >>= \(a, l1)        ->
   draws y l1               >>= \(b, l2)        ->
   draws z l2               >>= \(c@(n:_), foo) ->
   guard (null foo)         >>
   guard (extract n /= 0)   >>
   let [n1, n2, n3] = map (asNum . N) [a, b, c] in
   guard (n1 * n2 == n3)    >>
   return (n1, n2, n3)

{-- Define multiplicationPuzzle so that x * y == z is true
>>> multiplicationPuzzle (N [Observed 7, free, free]) (N [Observed 4, free]) (N (replicate 5 free))
[(715,46,32890)]
>>> 715 * 46
32890

This is the brute-force method, in that this multiplicationPuzzle solves any
a * b == c problem (insofar as the problem consumes all digits), but we can also
help this along a bit. How so? Well, there are some constraints, that no digit
repeats, and we know some properties of numbers. Let's look at a little bit of
help here.

1. 7 * 4 = 28 so the left-most digit of the solution (c) is either 2 or 3
   (because of a possible carry)
2. Anything * 1 is itself, so the right-most digit of the multiplier/multiplicant
   cannot be 1 (because we cannot have repeats)
3. Same for 0 in that 0 * anything is 0, repeating the 0 in the right-most digit
   if it is there, so right-most of a and b cannot be 0.
4. Further we also know that no free value can be 4 nor 7. 7 is eliminated
   right away, but 4 is not (not for the first two free variables), so we can
   constrain those two, too!

Let's encodify that.
--}

eitherOr, neitherNor :: Int -> Int -> (Int -> Bool)
eitherOr a b = (||) . (== a) <*> (== b)
neitherNor n m = (&&) . (/= n) <*> (/= m)

andNot :: Int -> (Int -> Bool)
andNot = (/=)

{--
So now we assemble our logic into our query:

>>> multiplicationPuzzle (N [Observed 7,
                             constrain (andNot 4),
                             constrain ((&&) . neitherNor 0 1 <*> andNot 4)]) 
                         (N [Observed 4, constrain (neitherNor 0 1)])
                         (N (constrain (eitherOr 2 3):replicate 4 free))
[(715,46,32890)]

This went through 'all' the possibilities much faster because the constraints
eliminated many of the 'non-possibilities.'

Notice that all variables are constrained up toward the end. Constraining early
significantly speeds processing.
--}
