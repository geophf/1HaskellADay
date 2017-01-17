module Y2017.M01.D17.Exercise where

import Data.Traversable

{--
Following up from yesterday's exercise on our Data.Traversable-exploration via
@yoeight's blog post at the URL:

http://www.corecursion.net/post/2017-01-12-Why_Traversable_is_the_real_deal

Today we will be implementing minAF and maxAF over Traversable types.

That is to say:
--}

minAF, maxAF :: (Traversable t, Ord a) => t a -> a
minAF = undefined
maxAF = undefined

-- Question: are minAF and maxAF partial functions?

-- what are minAF and maxAF for:
        
nada, uno, rnds :: [Int]
nada = []
uno = [1]
rnds = [95902198162138,136017932361032,5827212341670,92864718312210,
        78260380050431,62591985123053,46614386344434,156252552367524,
        38573429178097,9798683366671]

-- rnds computed via: *Data.Random> rndSeed >>= evalStateT (gimme 10)

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

toMT :: (Traversable t, Ord a) => t a -> MetaTrav t a
toMT = undefined

-- What are the toMT values for nada, uno, and rnds?

-- so your maxT and minT functions are now O(1) for MetaTrav values. lolneat.
