module Y2021.M04.D05.Exercise where

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

data Vector a = Vect { sz :: Int, vals :: [a] }
   deriving (Eq, Ord, Show)

vempty :: Vector a
vempty = undefined

mkVect :: Traversable t => t a -> Vector a
mkVect = undefined

-- you need Functor Vector to make this work

instance Functor Vector where
   fmap = undefined

instance Foldable Vector where
   foldMap = undefined
   length = undefined    -- don't use the default implementation

instance Traversable Vector where
   traverse = undefined

-- and why not Monoid? Why not, indeed!

instance Monoid (Vector a) where
   mempty = undefined
   mappend = undefined

-- Now that we have Vector, let's declare a safe-take function

vtake :: Int -> Vector a -> Maybe (Vector a)
vtake = undefined

{--
>>> vtake 50 (mkVect [1,2,3])
Nothing

>>> vtake 1 (mkVect [1,2,3])
Just (Vect {sz = 1, vals = [1]})
--}

-- What does this function return?

hey = mapM print (mkVect [1,2,3])

-- also, you get size-adjustments for free, too, right?

vappend :: Vector a -> Vector a -> Vector a
vappend = undefined
