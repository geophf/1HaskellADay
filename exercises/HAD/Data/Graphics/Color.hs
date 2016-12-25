module Data.Graphics.Color where

import Control.Applicative
import Numeric

-- Below imports available from 1HaskellADay git repository

import Control.Presentation
import Data.Graphics.BoundingBox
import Data.Graphics.SVG
import Data.XHTML

{-- a solution to the problem posted at http://lpaste.net/1249981301570666496

Last week's exercises around representing (graphically) data and then organizing
them revealed some interesting problems to solve. This week we'll be looking
at one way of representing data sets: color.

Now color can be represented as a set of named values:

Pink | Blue | PurpleRain

And there are plusses and minuses to doing that.

Or it can be represented as some numeric value or sets of numeric values.

But what works best for you?

Represent color:
--}

data Color a = RGB (a,a,a)
   deriving (Eq, Ord)

instance Show a => Show (Color a) where
   show (RGB colour) = "rgb" ++ show colour

instance Show a => Univ (Color a) where
   explode (RGB (r,g,b)) = map show [r,g,b]

{-- a solution to the problem posted at http://lpaste.net/3178349384714682368

Yesterday http://lpaste.net/9210044109090193408 (or just see the declaration
above) we saw how colors can be represented as a set of 'things' and the things 
we chose to represent a color were three numbers, representing quantified 
red/green/blue values.

Okay, so let's cement that today. Let's represent Color as a Num-instance.

In so doing, we have fromInteger, which means not only is Color a Num that
can be added and what-not, but that also that a Num (particularly an Integer)
can be a color and converted to as such.

--}

-- but what makes making Color a Num-instance easier is if it is already
-- a functor, so let's do that:

instance Functor Color where
   fmap f (RGB (a,b,c)) = RGB (f a, f b, f c)

-- with Color a Functor, we can now more easily define Color a as a Num

instance Num a => Num (Color a) where
   (+) = liftA2 (+)
   (*) = liftA2 (*)
   abs c = c
   signum c = c
   fromInteger color = fmap fromIntegral (RGB (r,g,b))
      where (c,b) = color `divMod` 256
            (r,g) = c     `divMod` 256
   negate color = undefined -- we'll look at this tomorrow

instance Applicative Color where
   pure = undefined -- \(RGB (a,b,c)) -> RGB (f1 a, f2 b, f3 c)
   RGB (f1, f2, f3) <*> RGB (a,b,c) = RGB (f1 a, f2 b, f3 c)

instance (Ord a, Num a) => Monoid (Color a) where
   mempty = RGB (0,0,0)
   mappend = liftA2 max -- obadz @obadzz #1Liner solution

complement :: Color Int -> Color Int
complement = fmap (255 -)

-- okay, so, let's show some circs! (not 'jerks': circs)

pink, blue, green :: Color Int
pink = RGB (255, 150, 150)
       -- from: http://www.rapidtables.com/web/color/pink-color.htm
blue = RGB (0, 0, 255)
green = RGB (0, 255, 0)

yourCircs :: Element
yourCircs =
   svgFromBB (makeBoundingBox (1,100) (0,0) (500,500))
      [circ 20 20 5 (show pink) [],
       circ 50 80 15 (show blue) [],
       circ 80 40 25 (show green) []]

colorIdx :: Color Int -> Int
colorIdx (RGB (x,y,z)) = ((x * 256) + y) * 256 + z

recolor :: Int -> Color Int
recolor idx = RGB (x,y,z)
   where (x, r) = idx `quotRem` 65536
         (y, z) = r `quotRem` 256

showColor :: Int -> String
showColor = ("'#" ++) . (++ "'") . (`showHex` "")
