module Y2016.M07.D19.Solution where

{--
A Trigon, also known in some circles as a 'triangle,' is a three-SIDED shape.
Trigons are have several interesting characteristics. A trigon also defines a
plane (in which it lies), and a set of trigons can be rendered efficiently these
days to represent, e.g. characters in 3 dimensions, such as pokémon, for example

... now that I have your attention.

Look at the figure tri2.gif at this directory or at the URL:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M07/D19/tri2.gif

Today's #haskell exercise is to declare the type Trigon, and then to compute
the number of trigons in that figure. Also compute the total area, because fun.
--}

import Data.Map (Map)
import qualified Data.Map as Map

data Trigon = A3SidedPolygon deriving Show

type Point2d = (Float, Float)

type Figure = Map Char Point2d

figure2 :: Figure
figure2 = Map.fromList (zip "abgcdthfjkmnp"
   [(0,0), (15,10),(25,10),(35,10),(50,0),
    (35,-10),(25,-10),(15,-10),
    (15,0),(20,0),(25,0),(30,0),(35,0)])

countingTrigons :: Figure -> Int
countingTrigons = undefined

-- hint: it is possible for trigons to overlap or to contain other trigons
-- within them

{-- BONUS -----------------------------------------------------------------

The area of a trigon is its  bh / 2
    where b = length of the base of the trigon
          h = height of the trigon

Of course the area of a 'square' is the square of the length of its side ...
that's why a square is called 'square,' you see.

But I digress ... or do I?

What is the area of the figure?
--}

area :: Figure -> Float
area = undefined

-- BONUS-BONUS: why is area called 'area'? What is its etimology?

-- The figure is figure2 because we'll do a bit of exploration with shapes
-- this week.
