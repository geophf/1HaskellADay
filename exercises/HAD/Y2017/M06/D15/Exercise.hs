module Y2017.M06.D15.Exercise where

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

instance Show t => Show (L t r) where
   show Nil = undefined
   show (Cons t r) = undefined

-- hint: there may be a trick to this show instance, a setting-up context.

-- Problem 2.

-- write headμ and tailμ functions

headμ :: List t -> t
headμ = undefined

tailμ :: List t -> List t
tailμ = undefined

-- Of course, like head and tail, these are not total functions.
-- Consolation: deal with it.

-- Problem 3.

-- Of course, List t is Foldable and Traversable, right?
-- Make it so, Number 1

instance Foldable (L t) where
   foldMap = undefined

instance Traversable (L t) where
   traverse = undefined -- or you can define sequenceA if you prefer

-- which means, OF COURSE, that List t MUST BE a Functor!

instance Functor (L t) where
   fmap = undefined

-- Problem 4.

-- Now that you have all that, define a List Int of 1 .. 10. What is the sum of
-- that list's elements's successors? What is the headμ? What is the tailμ?

oneToTen :: List Int
oneToTen = undefined

sumSucc10 :: List Int -> Int
sumSucc10 = undefined

headμ10 :: List Int -> Int
headμ10 = undefined

tailμ10 :: List Int -> List Int
tailμ10 = undefined