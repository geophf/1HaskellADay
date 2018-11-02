module Y2018.M11.D02.Exercise where

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

type Odd = Integer
type Square = Integer

twoSquaresDifference :: Odd -> (Square, Square)
twoSquaresDifference oddNumber = undefined
