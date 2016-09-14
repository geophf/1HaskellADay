module Y2016.M09.D14.Exercise where

import Data.Map (Map)

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

solver :: [Domino] -> [Map Char Int]
solver tiles = undefined
