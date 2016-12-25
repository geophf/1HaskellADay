{-# LANGUAGE ViewPatterns #-}

module Graph.ScoreCard.ColorScheme where

-- http://lpaste.net/5433500771834396672
-- What we do here is provide a coloring scheme for a set of score cards.

import Control.Arrow
import Data.Array
import Data.Foldable

import Control.List (minmax)          -- http://lpaste.net/107211
import Data.Graphics.Color            -- http://lpaste.net/9210044109090193408
import Graph.ScoreCard                -- http://lpaste.net/7322735479504240640 

colorize :: (Foldable t, Ix a, Ix b, Ord c, RealFrac c)
         => t (ScoreCard a b c) -> Array a (Color Int)
colorize = uncurry array . (minmax . map fst &&& id)
         . uncurry (map . second) . first intensify
         . foldr (uncurry (***) . (mappend . snd &&& (:)) . (idx &&& color))
                 (mempty, [])
         . toList

-- To colorize scorecards:

-- Actually, for n-vectors where n is greater than 3, there is a really
-- straight-foward approach, we just divvy the elements into three buckets
-- then create a number to represent each bucket to represent the RGB-triple

-- Now, OTOH, if the n-vector is n < 3 then we have to do something else, now
-- don't we, skippy?

color :: (Ix b, Num c, RealFrac c) => ScoreCard a b c -> Color c
color = RGB . combineComp . (`divisorF` ([],[],[])) . elems . values

combineComp :: Num a => ([a], [a], [a]) -> (a, a, a)
combineComp (sum -> a, sum -> b, sum -> c) = (a,b,c)

divisorF :: (RealFrac a, Num a) => [a] -> ([a], [a], [a]) -> ([a], [a], [a])
divisorF list@(x:y:z:r) = divisorF r . addOn (x, y, z)
divisorF list@[x,y]     = addOn (x, (x + y) / 2, y)
divisorF list@[x]       = addOn (x, x, x)
divisorF []             = id

addOn :: (a,a,a) -> ([a],[a],[a]) -> ([a],[a],[a])
addOn (x,y,z) (a,b,c) = (x:a,y:b,z:c)

{--
For intensity, if we accumulate intensity as we go, then we simply just translate a
scorecard vector into its color components and translate as needed.
--}

type Intensity a = Color a

intensify :: RealFrac a => Intensity a -> Color a -> Color Int

-- for this function to work, Color has to be a functor ... that's not hard...

intensify (RGB (r,g,b)) (RGB (r1, g1, b1)) =
   RGB (cmap r r1, cmap g g1, cmap b b1)

cmap :: RealFrac a => a -> a -> Int
cmap intense = floor . (+) 0.5 . (*) (255.0 / intense)

-- *Graph.Cell> intensify 1.2 (RGB (0.5,1.2,0.8)) ~> rgb(106,255,170)
