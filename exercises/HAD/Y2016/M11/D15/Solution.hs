{-# LANGUAGE TupleSections #-}

module Y2016.M11.D15.Solution where

import Control.Applicative
import Control.Monad
import Data.Monoid

-- below import available from 1HaskellADay repository

import Control.Logic.Frege
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

data Unity x = U x | Fail
   deriving (Eq, Show)

instance Functor Unity where
   fmap f Fail = Fail
   fmap f (U x) = U (f x)

instance Applicative Unity where
   pure = U
   U f <*> U x = U (f x)
   _   <*> _   = Fail

instance Monad Unity where
   return = U
   Fail >>= f = Fail
   (U x) >>= f = f x

instance {-- Monoid x => --} Monoid (Unity x) where
   mempty = Fail
   Fail `mappend` u2 = Fail
   _    `mappend` Fail = Fail
   U x  `mappend` U y = U x -- (x `mappend` y)

-- does requiring x to be monoidal simplify the monoidal definition?
-- does requiring x to be monoidal complicate the monoidal Unity-use?

-- ... yeah, I'm relaxing the monoidal constraint on the funked-value
-- which complicates the monoid definition of Unity with a conceit that
-- first is preferable

instance Alternative Unity where
   empty = Fail
   Fail <|> u2 = u2
   u1   <|> _  = u1

instance MonadPlus Unity where
   mzero = Fail
   Fail `mplus` u2 = u2
   u1   `mplus` _  = u1

unify :: (Eq a, MonadPlus m, Monoid (m (Maybe a))) =>
         Maybe a -> Maybe a -> m (Maybe a)
unify ans@(Just x) (Just y) = x == y -| return ans
unify Nothing x = return x
unify x _       = return x

-- hint: does it 'work' when unification fails because Just 3 /= Just 4?

{-- let's see:
*Y2016.M11.D15.Solution> unify (Just 3) (Just 4) :: Unity (Maybe Int)
Fail
*Y2016.M11.D15.Solution> unify (Just "Amy") Nothing :: Unity (Maybe String)
U (Just "Amy")
*Y2016.M11.D15.Solution> unify (Just 3) (Just 3) :: Unity (Maybe Int)
U (Just 3)
*Y2016.M11.D15.Solution> unify Nothing (Just 3) :: Unity (Maybe Int)
U (Just 3)

Yup. Looks good.
--}

-- Exercises: unify (or don't unify) the following. How would you go about it?

type Name = String

data Human = Pers (Maybe Name) (Maybe Role)
   deriving (Eq, Show)

roles :: [Role]
roles = [Husband, Mother, DaughterInLaw, Sister]

toUnify :: [(Human, Human)]
-- actually, the argument runs thus:
toUnify = map (Pers (Just "Amy") (Just DaughterInLaw),)
              (map (Pers Nothing . Just) roles)

-- and the we can match anybody else with anybody else:

-- ++ [(Pers Nothing (Just x), Pers Nothing (Just y)) | x <- roles, y <- roles]

{-- not this:
toUnify = [(Pers x y, Pers a b) | x <- [Nothing, Just "Amy"],
                                  a <- [Nothing, Just "Amy"],
                                  y <- [Husband, Mother, DaughterInLaw, Sister],
                                  b <- [Husband, Mother, DaughterInLaw, Sister]]
--}

{--
The problem of Amy: Amy's role changes, based on her relationship to the person
compared. To her mother-in-law, she is a daughter-in-law. To her husband, she
is a wife. To her daughter, she is a mother. Hm.

PEOPLE ARE COMPLICATED AND HARD THINGS!

(I just learned this, today ... yes.)
--}

unifyPeeps :: Human -> Human -> Unity Human
unifyPeeps (Pers n1 r1) (Pers n2 r2) =
   liftA2 Pers (unify n1 n2) (unify r1 r2)

-- *Y2016.M11.D15.Solution> map (uncurry unifyPeeps) toUnify ~>
-- [Fail,Fail,U (Pers (Just "Amy") (Just DaughterInLaw)),Fail]
