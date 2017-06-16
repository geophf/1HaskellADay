{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Y2017.M06.D15.Solution where

import Data.Monoid ((<>))

{--
Today's Haskell problem comes by way of @aisamanra, Mr. Lists-are-Mu-functions-
in-the-function-of-Mus.

For, as we all know: 'Mus' is the plural of 'Mu.'

Deep thoughts, from geophf, the Muse.

geddit? Mu? Mus? Muse? GEDDIT?

Anyway.

'getty the sitokouros' defines Lists as follows:

(also see hipster-lists.png)
--}

newtype Mu f = Mu (f (Mu f))
data L t r = Nil | Cons t r
type List t = Mu (L t)

-- now why @aisamanra didn't have the type be Μ and not Mu ... that is to say:
-- Μ, the Greek letter for M, which you often see as μ, is beyond me, but okay.

-- So, anyway, today's haskell problem

-- Problem 1.

-- Scare your friends, and replace all [a] values with List a values, but write
-- a show-instance so that they can't tell the difference ... AT ALL!

instance Show t => Show (List t) where
   show = showL

showL :: Show t => List t -> String
showL (Mu Nil) = "[]"
showL (Mu (Cons t r)) = '[' : show t ++ showing r

showing :: Show t => List t -> String
showing (Mu Nil) = "]"
showing (Mu (Cons t r)) = ", " ++ show t ++ showing r

-- hint: there may be a trick to this show instance, a setting-up context.

{--
>>> Mu Nil
[]
>>> Mu (Cons 1 (Mu (Cons 2 (Mu (Cons 3 (Mu Nil))))))
[1, 2, 3]
--}

-- Problem 2.

-- write headμ and tailμ functions

headμ :: List t -> t
headμ (Mu (Cons x _)) = x

tailμ :: List t -> List t
tailμ (Mu (Cons _ r)) = r

-- Of course, like head and tail, these are not total functions.
-- Consolation: deal with it.

{-- eh, types to hard for my head!

-- Problem 3.

-- Of course, List t is Foldable and Traversable, right?
-- Make it so, Number 1

instance Foldable f => Foldable (L f) where
   foldMap f Nil = mempty
   foldMap f (Cons x r) = f x <> foldMap f r

instance Traversable (L t) where
   traverse = undefined -- or you can define sequenceA if you prefer

-- which means, OF COURSE, that List t MUST BE a Functor!

instance Functor (L t) where
   fmap = undefined
--}

-- Problem 4.

-- Now that you have all that, define a List Int of 1 .. 10. What is the sum of
-- that list's elements's successors? What is the headμ? What is the tailμ?

oneToTen :: List Int
oneToTen = fromList [1..10]

fromList :: [a] -> List a
fromList [] = Mu Nil
fromList (h:t) = Mu (Cons h (fromList t))

{--
>>> oneToTen 
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

I'M TOTALLY FOOLED!
--}

-- sum is a fold

sumSucc10 :: List Int -> Int
sumSucc10 = foldµ (+) 0 . mapµ succ

foldµ :: (a -> b -> b) -> b -> List a -> b
foldµ f z (Mu Nil) = z
foldµ f z (Mu (Cons x r)) = foldµ f (f x z) r

mapµ :: (a -> b) -> List a -> List b
mapµ f (Mu Nil) = Mu Nil
mapµ f (Mu (Cons x r)) = Mu (Cons (f x) (mapµ f r))

{--
>>> sumSucc10 oneToTen 
65
--}

headμ10 :: List Int -> Int
headμ10 = headμ

{--
>>> headμ10 oneToTen 
1
--}

tailμ10 :: List Int -> List Int
tailμ10 = tailμ

{--
>>> tailμ10 oneToTen 
[2, 3, 4, 5, 6, 7, 8, 9, 10]

n.b.: the type of the value returned is List Int, not [Int]
--}
