module Y2016.M11.D15.Exercise where

import Data.Monoid

-- below import available from 1HaskellADay repository

import Y2016.M11.D14.Exercise

{--
It's a question of unification.

Yesterday, we had a 'simple' logic puzzle. A conundrum arose, however, when
the value AMY also happened to be the same person as (Person DaughterInLaw).

How can we show these two things are equal?

Well, structurally, they are not, so to equate them would be a major twist
on the Eq-instance and then have possibly major consequences.

So, ick!

But SCIENCE to the rescue.

There is a thing in logical programming called unification. That does not
solve our problem at all, but if we look through the problem through a logic
lens we can see that we can use unification with a different structure

If we redeclare Person to be

Person (Maybe Name) Role

then we take a definition of unification to be 'Nothing unifies with a Just-
value and is promoted to that Just-value,' then we no longer strain at
redefining equality.

So, let's look at some simple rules for a simply unifier.

1. Just x unifies with Just y if x == y
2. Nothing unifies with Just x IN THE UNIFICATION MONAD (monad? applicative?)
3. Nothing unifies with Nothing

1. and 3. are simple enough. What about that 2.?

Today's Haskell problem: define simple (not occurs-checking) unification for
the Maybe-type.
--}

data Unity x = U x

instance Functor Unity where
   fmap f u = undefined

instance Applicative Unity where
   pure = undefined
   (<*>) = undefined

instance Monad Unity where
   return x = undefined
   u >>= f = undefined

instance Monoid x => Monoid (Unity x) where
   mempty = undefined
   u1 `mappend` u2 = undefined

-- does requiring x to be monoidal simplify the monoidal definition?
-- does requiring x to be monoidal complicate the monoidal Unity-use?

unify :: Monad m => Maybe a -> Maybe a -> m (Maybe a)
unify x y = undefined

-- hint: does it 'work' when unification fails because Just 3 /= Just 4?

-- Exercises: unify (or don't unify) the following. How would you go about it?

type Name = String

data Human = Pers (Maybe Name) Role
   deriving (Eq, Show)

toUnify :: [(Human, Human)]
toUnify = [(Pers x y, Pers a b) | x <- [Nothing, Just "Amy"],
                                  a <- [Nothing, Just "Amy"],
                                  y <- [Husband, Mother, DaughterInLaw, Sister],
                                  b <- [Husband, Mother, DaughterInLaw, Sister]]
