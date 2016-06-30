module Control.DList where

{--
Your basic difference list, from Haskell to Idris, and back
This answers the Haskell exercise posed at http://lpaste.net/107593
--}

import Control.Arrow (app)
import Control.Monad
import Control.Monad.Trans.Writer
import Data.Monoid

infixr 5 |>
infixl 5 <|

data DList a = DL { unDL :: [a] -> [a] }

cons :: a -> [a] -> [a]
cons a list = a : list

(<|) :: DList a -> a -> DList a
dlist <| a = DL ((unDL dlist) . (cons a))

(|>) :: a -> DList a -> DList a
a |> dlist = DL ((cons a) . (unDL dlist))

emptyDL :: DList a
emptyDL = DL id

dlToList :: DList a -> [a]
dlToList dlist = unDL dlist []

-- added a fromList for difference lists to start from somewhere

dl :: [a] -> DList a
dl = DL . (++)

dl' :: a -> DList a -- your monadic return
dl' x = dl [x]

-- DLists are monoidal (perfect for writing to *cough* Writer *cough*)

instance Monoid (DList a) where
   mempty  = emptyDL
   d1 `mappend` d2 = DL (unDL d1 . unDL d2)

samp :: Writer (DList Char) () -- from Writer w a
samp = (\f twoB -> mapM_ f [twoB, " or not ", twoB]) (tell . dl) "to be"

-- *Control.DList> dlToList $ snd $ runWriter samp ~> "to be or not to be"

-- So now let's make DList a monad
-- which means we have to make it a Functor, then an Applicative one
-- You know: because monads are functors.
-- No, they aren't but in Haskell they are required to be, because reasons.

instance Monad DList where
   return = pure
   difflist >>= f = {-- ugh! --} mconcat . map f $ dlToList difflist

-- but to be a monad these days, you MUST be applicative, so ...

instance Applicative DList where
   pure = dl'
   dlfs <*> dlargs = dl $ zipWith (curry app) (dlToList dlfs) (dlToList dlargs)

-- but applicative needs to be a functor, so ...

instance Functor DList where
   fmap f = dl . map f . dlToList

-- and while we're at it, let's make DList a Foldable instance
{--
Okay, last week, we looked at improving the views on DList so that we could
make it a monad, so that it could participate with MultiMap, which has a
requirement of monadicity on the element-container-class.

But is that the 'right' improvement?

Looking at MultiMap, it's not so that a contain should be a monad, because
what is the usage for these containers? Their monadicity?

No, it's their foldability.

This exercise is inspired by @argumatronic xxx.

What is a Foldable instance?

(quick lookup of Data.Foldable)

A Foldable instance is something that which can be folded.

(audience, sarcastically: Thank you, geophf)

me: yer welcks.

So, (... or maybe we need to ba Traversable, too?)

(... I'll put more thought into this ... laterz).

So, if what we need, really, is a Foldable instance (not a Monad), then we need
to make DList a Foldable instance, too.

Today's Haskell exercise: declare DList to be Foldable and define the Foldable
functions for it. --}

instance Foldable DList where
   foldr f z = foldr f z . dlToList

-- let's talk Traversibility, now:

{-- a solution to the problem posted at http://lpaste.net/4868288594713772032

So, we made DList a Foldable instance, all for @argumatronic, and she was 
grateful for the source material for @HaskellBook ...

(geophf waits for the influx of gratitude)

... but then she was all like, "Phfeh! Foldable is so yesterday! Traversable
is where it's at! Get with the times!"

So, there's that.

So, Traversable. What is it?

traversable, adj.: that which can be traversed, see traversable.

(audience, sarcastic: gee! thanks, geophf!)

(geophf: yer welcks)

Okay, but seriously, looking at the documentation for Data.Traversable, we see
that 

class (Functor t, Foldable t) => Traversable t where ...

Well, as we've done yesterday and the proceeding week, we've already made DList
a Functor (and an Applicative Functor, no less, which will come in (very)
handy today) and, yesterday, made it Foldable, so we've met the prerequisites
of being able to make DList Traversable.

What is the power of traversibility? Well, if you have some arbitrary type, t,
that's a Traversable type, then you know you can map(M) or sequence over it...

... and as we learned in the '70s, being able to map over a type is a very
powerful and useful thing to have.

So, let's do this for DList: let's make DList a Traversable instance.

Generally, with Applicative Functors, as the minimally-complete definition
is traverse or sequenceA.  Let's do this. --}

instance Traversable DList where
   traverse f = (pure dl <*>) . traverse f . dlToList

-- And if we just want a feelz, yo:

instance Show (DList a) where show = const "a DList"
