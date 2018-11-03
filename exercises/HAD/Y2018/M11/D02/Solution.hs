module Y2018.M11.D02.Solution where

{--
From @fermatslibrary:

Every odd integer is the difference of 2 squares
Proof:
1) Pick an odd number
░░ ░░ ░░ ░░ ░░

2) Bend it in half
░░ ░░ ░░
░░
░░

3) Fill in the rest
░░ ░░ ░░
░░ ▓▓ ▓▓
░░ ▓▓ ▓▓

The odd nbr is the area difference of the big and small squares

tweet url: https://twitter.com/fermatslibrary/status/1057614426273132546

Let's prove this.

For any given odd number, odd, return the two squares where that odd number
is their difference:
--}

import Control.Monad (guard)

type Odd = Integer
type Square = Integer

twoSquaresDifference :: Odd -> Maybe (Square, Square)
twoSquaresDifference oddNumber =
   let root1 = oddNumber `div` 2
       root2 = succ root1 in
   guard (root2 * root2 - root1 * root1 == oddNumber) >>
   return (root2 * root2, root1 * root1)

{--
>>> twoSquaresDifference 5
Just (9,4)
>>> twoSquaresDifference 177
Just (7921,7744)
>>> twoSquaresDifference 177838794398758287756773
Just (7906659198300955535836869758956544336245721769,7906659198300955535836691920162145577957964996)
--}
