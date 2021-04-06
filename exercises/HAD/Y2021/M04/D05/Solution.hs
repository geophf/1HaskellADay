module Y2021.M04.D05.Solution where

{--
Today we're going to (re)visit Foldable and Traversable with our trusty old
Vector-type.

The Vector-type is a list of a fixed size:

Vector 0 []
Vector 1 [foo]
Vector 2 [foo, bar]

...

So, the Foldable function, length, comes for free.

Create a data-type, Vector, and make it foldable and traversable, so
that you can map or sequence over it:
--}

import Data.Foldable (toList)

import Control.Logic.Frege ((-|))

data Vector a = Vect { sz :: Int, vals ::  [a] }
   deriving (Eq, Ord, Show)

vempty :: Vector a
vempty = Vect 0 []

mkVect :: Traversable t => t a -> Vector a
mkVect = Vect . length <*> toList

-- you need Functor Vector to make this work

instance Functor Vector where
   fmap f (Vect n v) = Vect n (fmap f v)

instance Foldable Vector where
   foldMap f = foldMap f . vals
   length = sz

instance Traversable Vector where
   traverse f (Vect n v) = Vect n <$> traverse f v

-- Now that we have Vector, let's declare a safe-take function

instance Monoid (Vector a) where
   mempty = vempty
   mappend = vappend

vtake :: Int -> Vector a -> Maybe (Vector a)
vtake n (Vect m v) = n <= m -| Just (Vect n (take n v))

{--
>>> vtake 50 (mkVect [1,2,3])
Nothing

>>> vtake 1 (mkVect [1,2,3])
Just (Vect {sz = 1, vals = [1]})
--}

-- What does this function return?

hey = mapM print (mkVect [1,2,3])

{--
>>> hey 
1
2
3
Vect {sz = 3, vals = [(),(),()]}
--}

-- also, you get size-adjustments for free, too, right?

vappend :: Vector a -> Vector a -> Vector a
vappend (Vect n1 v1) (Vect n2 v2) = Vect (n1 + n2) (v1 ++ v2)

-- Of course, if we used difference lists, then v1 ++ v2 would be O(c)
-- but then, fmap &c become ... 'interesting.' idk.
