module Y2016.M09.D14.Solution where

import Control.Arrow (first, (>>>))
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map

-- below import available at 1HaskellADay git repository

import Control.List (takeout)
import Control.Logic.Frege ((<<-))

{--
This problem is from Master's Varity Puzzles from PennyPress, page 33.

Today we'll do a little domino arithmetic.

A domino tile is a tile with the playing side divided in half with each half
marked with zero up to 6 (inclusive) marks

https://en.wikipedia.org/wiki/Dominoes

So, an example tile:

   _________________
   | *   * | * * * |
   |   *   |       |
   | *   * | * * * |
   -----------------

is the (5,6) domino.

As a domino can be play in any direction, the (5,6) and (6,5) domino are the
same domino.

There's a hint for you.

Okay, we're not going to play a game of dominos (not today, anyway), we're
going to use some domino tiles in an unorthodox manner.

Given the following dominos:
--}

type Domino = (Int, Int)

dominos :: [Domino]
dominos = [(5,6), (2,5), (6,6), (4,2)]

{-- 
Now, here's the unorthodoxy: we're going to align the dominos so that they form 
decimal numbers and perform 'domino multiplication

Here's the alignment:

       (x, y) (z,
    *          a)
    -------------
    (b, c) (d, e)

That is to say, domino (x,y) forms the 100 and 10 digits of the multiplicand
the second domino (z, a) forms the 1's digit of the multiplicand and also is
the 1's digit of the multiplier.

The other two dominos (b,c) and (d,e) form the solution. So

xyz * a = bcde

Solve this problem for positions "xyzabcde" with the dominos supplied:
--}

-- converts d1, d2 to xyz, a
twoDominos2numbers :: Domino -> Domino -> (Int, Int)
twoDominos2numbers (x, y) (z, a) = (x * 100 + y * 10 + z, a)

-- *Y2016.M09.D14.Solution> twoDominos2numbers (5,6) (4,2) ~> (564,2)

twoDominosProduct :: Domino -> Domino -> Int
twoDominosProduct = (first (10*) >>> uncurry (+)) <<- twoDominos2numbers

-- *Y2016.M09.D14.Solution> twoDominosProduct (5,6) (4,2) ~> 5642

-- So, we can brute-force this:

solver :: [Domino] -> [Map Char Int]
solver tiles = takeout tiles >>= \(d1@(x,y), t1) ->
   takeout t1 >>= \(d2@(z,a), t2) ->
   takeout t2 >>= \(d3@(b,c), [d4@(d,e)]) ->
   guard (uncurry (*) (twoDominos2numbers d1 d2) == twoDominosProduct d3 d4) >>
   return (Map.fromList (zip "xyzabcde" [x,y,z,a,b,c,d,e]))

{--
*Y2016.M09.D14.Solution> solver dominos 
[fromList [('a',6),('b',2),('c',5),('d',5),('e',6),('x',4),('y',2),('z',6)]]
*Y2016.M09.D14.Solution> 426 * 6 ~> 2556
*Y2016.M09.D14.Solution> 426 * 6 == 2556 ~> True

n.b.: this solution just so happens to work for this problem's data set, but it
does NOT work where dominos must be flipped. Can you solve that more general 
problem?
--}
