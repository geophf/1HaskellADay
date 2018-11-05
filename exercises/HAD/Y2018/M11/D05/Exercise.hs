module Y2018.M11.D05.Exercise where

import Data.Set (Set)

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

data Domain = SomeRepresentationOfCalculatorFace

-- now from that domain, enumerate all the rectanglur numbers

type Digit = Int
type RectangularNumber = (Digit, Digit, Digit, Digit)

allRectangularNumbers :: Domain -> Set RectangularNumber
allRectangularNumbers calculator = undefined

-- How many rectangular numbers are there?

-- Next, partition the rectangular numbers into two heaps: one that has numbers
-- divisible by 11 and the other that has numbers that are not divisible by 11.

partitionBy11 :: Set RectangularNumber -> (Set RectangularNumber, Set RectangularNumber)
partitionBy11 rectangularNumbers = undefined

-- What are the sizes of the two sets?

-- You may wish to have a function that converts a RectangularNumber to an Int

rect2Int :: RectangularNumber -> Int
rect2Int rect = undefined
