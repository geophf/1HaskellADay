module Y2017.M01.D17.Solution where

import Control.Arrow ((***))
import Data.Traversable

{--
Following up from yesterday's exercise on our Data.Traversable-exploration via
@yoeight's blog post at the URL:

http://www.corecursion.net/post/2017-01-12-Why_Traversable_is_the_real_deal

Today we will be implementing minAF and maxAF over Traversable types.
--}

-- Well, following the theme of the blogpost:

newtype Min a = Min { mini :: a }

instance (Bounded a, Ord a) => Monoid (Min a) where
   mempty = Min maxBound
   mappend (Min a) (Min b) = Min (min a b)

newtype Max a = Max { maxi :: a }

instance (Bounded a, Ord a) => Monoid (Max a) where
   mempty = Max minBound
   mappend (Max a) (Max b) = Max (max a b)

minAF, maxAF :: (Traversable t, Ord a, Bounded a) => t a -> a
minAF = mini . foldMap Min
maxAF = maxi . foldMap Max

-- Question: are minAF and maxAF partial functions?

-- what are minAF and maxAF for:

nada, uno, rnds :: [Int]
nada = []
uno = [1]
rnds = [95902198162138,136017932361032,5827212341670,92864718312210,
        78260380050431,62591985123053,46614386344434,156252552367524,
        38573429178097,9798683366671]

-- rnds combputed via: *Data.Random> rndSeed >>= evalStateT (gimme 10) 

{--
*Y2017.M01.D17.Solution> [minAF] <*> [nada, uno, rnds]
[9223372036854775807,1,5827212341670]
*Y2017.M01.D17.Solution> [maxAF] <*> [nada, uno, rnds]
[-9223372036854775808,1,156252552367524]
--}

{-- BONUS -----------------------------------------------------------------

Fun fact: there's a trick question on google interviews that asks: what
is an efficient algorithm for finding the min and the max values in a large
list, where "large" means billions or trillions of elements.

Answer: O(1), they are lookups. Somebody had to construct those lists, either
you, in which case you retain those min and max values as you construct them,
knowing that question is asked frequently, or if somebody else constructs them,
require those meta-data in the delivery, as you are the customer and therefore
get to set the requirements.

So, given Traversable t, construct MetaTrav t such that:
--}

data MetaTrav t a = MT { minT, maxT :: a, struct :: t a }
   deriving Show

-- So now we can construct a MetaTrav from any Traversable type. How?

toMT :: (Traversable t, Bounded a, Ord a) => t a -> MetaTrav t a
toMT = uncurry MT . foldr (\e -> min e *** max e) (maxBound, minBound) <*> id

{--
*Y2017.M01.D17.Solution> mapM_ (print . toMT) [nada, uno, rnds]
 MT {minT = 9223372036854775807, maxT = -9223372036854775808, struct = []},
 MT {minT = 1, maxT = 1, struct = [1]},
 MT {minT = 5827212341670, maxT = 156252552367524,
     struct = [95902198162138,136017932361032,5827212341670,92864718312210,
               78260380050431,62591985123053,46614386344434,156252552367524,
               38573429178097,9798683366671]}

so your maxT and minT functions are now O(1) for MetaTrav values. lolneat.
--}
