module Y2018.M11.D05.Solution where

import Control.Arrow ((***))

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

{--
This is from @fermatslibrary tweet on calculator-rectangular numbers:

https://twitter.com/fermatslibrary/status/1052544515632959488

It is put forward for every 'rectangular number' from the number-grid:

   7  8  9
   
   4  5  6

   1  2  3

is divisible by 11.

Let's prove that intuitionistically.

First off, we have to enumerate the rectangular numbers. If we pose that these
numbers are lying on some cartesian plane, then we have a set of four numbers
from the same four digits of the upper left quadrant:

7 -> 8 -> 5 -> 4  ->>  7854
8 -> 5 -> 4 -> 7  ->>  8547
5 -> 4 -> 7 -> 8  ->>  5478 and
4 -> 7 -> 8 -> 5  ->>  4785

but, importantly, not the numbers 7548 nor its variants.

So it's rectangles, not cross-stitch.

So, first off: come up with a representation of the domain from which you can
construct these rectangular numbers.
--}

type Digit = Int
type Index = Digit
type Domain = Array (Index,Index) Digit

calculator :: Domain
calculator = listArray ((1,1),(3,3)) [7,8,9,4,5,6,1,2,3]

-- okay, that was too easy. I love Haskell!

-- now from that domain, enumerate all the rectanglur numbers

type RectangularNumber = (Digit, Digit, Digit, Digit)

allRectangularNumbers :: Domain -> Set RectangularNumber
allRectangularNumbers calc =
   let idx = [1,2]
       s = succ
       adder = listArray (1,2) [[s, s . s], [s]] in
   Set.fromList [rect | x <- idx, y <- idx, ax <- adder ! x, ay <- adder ! y,
                        rect <- chooseFrom calc x y ax ay]

chooseFrom :: Domain -> Index -> Index -> (Index -> Index) -> (Index -> Index)
           -> [RectangularNumber]
chooseFrom calc x y sx sy =
    [(calc ! (x,y), calc ! (sx x,y), calc ! (sx x, sy y), calc ! (x,sy y)),
     (calc ! (sx x,y), calc ! (sx x, sy y), calc ! (x,sy y), calc ! (x,y)),
     (calc ! (sx x, sy y), calc ! (x,sy y), calc ! (x,y), calc ! (sx x,y)),
     (calc ! (x,sy y), calc ! (x,y), calc ! (sx x,y), calc ! (sx x, sy y)),

-- that's one direction, counter-clockwise, now let's go the other direction

     (calc ! (x,y),calc ! (x, sy y), calc ! (sx x, sy y), calc ! (sx x, y)),
     (calc ! (x, sy y), calc ! (sx x, sy y), calc ! (sx x, y), calc ! (x,y)),
     (calc ! (sx x, sy y), calc ! (sx x, y), calc ! (x,y), calc ! (x,sy y)),
     (calc ! (sx x, y), calc ! (x,y), calc ! (x,sy y), calc ! (sx x, sy y))]

-- How many rectangular numbers are there?

{--
>>> calc = allRectangularNumbers calculator
>>> length calc
72
--}

-- are the following rectangular numbers in that set?

inRectangularSet :: Set RectangularNumber -> [RectangularNumber] -> Bool
inRectangularSet rects = all (`Set.member` rects)

-- use the following rectangular numbers as a sample set:

samples :: [RectangularNumber]
samples = [(5,6,3,2),(2,8,7,1)]

{--
>>> inRectangularSet calc samples
True
--}

-- Next, partition the rectangular numbers into two heaps: one that has numbers
-- divisible by 11 and the other that has numbers that are not divisible by 11.

partitionBy11 :: Set RectangularNumber -> (Set RectangularNumber, Set RectangularNumber)
partitionBy11 = Set.partition ((== 0) . (`mod` 11) . rect2Int)

-- What are the sizes of the two sets?

-- You may wish to have a function that converts a RectangularNumber to an Int

rect2Int :: RectangularNumber -> Int
rect2Int (a,b,c,d) = a * 1000 + b * 100 + c * 10 + d

{--
>>> (length *** length) (partitionBy11 calc)
(72,0)

All the rectangular numbers are divisible by 11. Q.E.D.
--}
