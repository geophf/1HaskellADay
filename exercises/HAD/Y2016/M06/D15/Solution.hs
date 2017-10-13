module Y2016.M06.D15.Solution where

{--
I'm sure there are many elegant ways to define the identity matrix.

I'll use Rule 16 from the rules of 2-dimensional cellular automata.
--}

import Data.Monoid

import Control.Automata.Cellular (runRule, genRule, seed)
import Control.Logic.Frege ((-|))
import Data.Matrix (Matrix, fromLists, pprint, ex1, cross, dims)
import Data.Stream (takeS)
import Data.Universe (toList)

identity :: Num a => Int -> Matrix a
identity n = fromLists (map (map (fromIntegral . fromEnum) . toList 0 n) cells)
   where cells = takeS n (runRule (genRule 16) seed)

{--
*Y2016.M06.D15.Solution> pprint (identity 5)
Matrix 5x5
| 1 0 0 0 0 |
| 0 1 0 0 0 |
| 0 0 1 0 0 |
| 0 0 0 1 0 |
| 0 0 0 0 1 |

Now show that the identity matrix is the identity matrix by crossing it with
ex1, for example. Generate the correctly sized identity matrix to cross with
any input matrix:
--}

instance Monoid (Matrix a) where
   mempty             = error "No null-matrix defined"
   mempty `mappend` a = a
   a      `mappend` _ = a

sameMatrix :: (Eq a, Num a) => Matrix a -> Matrix a
sameMatrix mat =
   let (l,r) = snd (dims mat) in
   mat `cross` identity l == mat && identity r `cross` mat == mat -| mat

{--
*Y2016.M06.D15.Solution> pprint $ sameMatrix ex1
Matrix 2x2
|  2.0  5.0 |
|  1.0 -3.0 |

YAY!
--}
