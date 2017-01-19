module Y2017.M01.D18.Solution where

import Data.Monoid
import Data.Traversable

-- Below import available from 1HaskellADay git repository

import Data.Bag

{--
Today's Haskell exercise continues our exploration of Data.Traversable from the
lovely blog entry by @yoeight at the URL:

http://www.corecursion.net/post/2017-01-12-Why_Traversable_is_the_real_deal

Today we'll be looking at summing elements of a Traversable type. The problem
with sum, qua sum, is that WAY before it was only for Data.List, but nowadays
it works on all Foldable types. So does that also mean sum works on all
Traversable types? Well, yes!

Let's look at the declaration of Traversable to see why:

class (Functor t, Foldable t) => Traversable (t :: * -> *) where

This says that every Traversable type, t, is also a Foldable type, t, as well.

So, sum works for a traversable type.

Let's prove this.

In the above import we have Data.Bag.Bag, which you see is type-equivalent
to a Map.  AND Map is traversable! Well, ...
--}

data Pet = Cat | Dog | Rabbit | Ferret | Fox | Fish
   deriving (Eq, Ord, Show)

pets :: Bag Pet
pets = fromList [Cat, Cat, Dog, Dog, Fish, Fish, Fish]

-- how many pets do you have?

-- *Y2017.M01.D18.Solution> sum pets ~> Sum {getSum = 7}

-- Question is Sum (x :: Int) a Num instance? Yes

{-- OKAY, NOW FOR REALZ, YO -------------------------------------------------

So, anyway, sum was a push-over. M'kay. Let's do a similar exercise then,
suggested by the same blog entry: average.
--}

instance (Real a, Num a) => Fractional (Sum a) where
   fromRational = Sum . nearestNum
   Sum x / Sum y = Sum (nearestNum (toRational x / toRational y))

nearestNum :: Num a => Rational -> a
nearestNum = fromIntegral . floor

averageAF :: (Fractional a, Traversable t) => t a -> a
averageAF = (/) . sum <*> fromIntegral . length

-- What are the average number of kinds of pets you have in the pets-bag?

-- *Y2017.M01.D18.Solution> averageAF pets ~> Sum {getSum = 2}
