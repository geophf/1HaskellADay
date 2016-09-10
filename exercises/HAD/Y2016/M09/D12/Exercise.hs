module Y2016.M09.D12.Exercise where

import Control.Comonad

-- below import available at 1HaskellADay git repository

import Control.List
import Data.Matrix

{--
Okay, so, logic puzzles: I love playing with them.

Here's another one from Master's Variety Puzzles from PennyPress, titled:
"Symbolic Logic" found on page 104 of this book.

You have this type:
--}

data Symbol = Asterisk | Hash | Infinity
   deriving (Eq, Ord, Enum)

instance Show Symbol where
  show Asterisk = "*"
  show Hash     = "#"
  show Infinity = "∞"

{-- 
and they are two in each row and column arranged in a grid
so, one such possible arrangment is as below:

*Y2016.M09.D12.Exercise> pprint . fromLists $ take 6 (cycle [Asterisk ..] =>> take 6)
Matrix 6x6

column  A B C D E F
row
 1    | * # ∞ * # ∞ |
 2    | # ∞ * # ∞ * |
 3    | ∞ * # ∞ * # |
 4    | * # ∞ * # ∞ |
 5    | # ∞ * # ∞ * |
 6    | ∞ * # ∞ * # |

But this is not the only arrangement, particularly if there are a set of 
constraints on the layout of these symbols.

Today's Haskell problem is to find the layout given the following constraints:

Row 1 is the only row with adjacent ∞s.
In row 2 no adjaent squares are the same symbol.
In row 3 the ∞s are somwhere to the left of the #s.
In row 4 the #s are somwhere to the left of the ∞s.
Row 6 is a palindrome.

In column A the *s are somehwere between the ∞s.
In column B no adjacent squares are the same symbol.
Column D is the only column with adjacent ∞s.
In column E the # are somwhere above the *s.
In column F each # is immediately above an ∞.
--}

layout :: Matrix Symbol
layout = undefined

-- Hint: eheh. 'Hint'? This is a puzzle! There are not hints! Go to! Go to!
