module Y2017.M01.D16.Exercise where

import Control.Applicative
import Data.Traversable

{--
"Let's talk about sex, baby! Let's talk about you and me!"

s/sex/Traversable/, but okay.

This week we'll be following along with the Traversable article by @yoeight.

So, read that, as you will: 

http://www.corecursion.net/post/2017-01-12-Why_Traversable_is_the_real_deal

As you can see: Traversable is the Real Deal (tm)

Why?

Well, 

1) since everything in Haskell is a List, and Traversable traverses things
that are essentially lists, then, yeah, it's the real deal.

(Pause, for effect <<- effectual computation, and watch all the Haskell
purists lose their [redacted] minds at my last statement)

and 2) Let's see.

So, from the article, we have the First data type and the find-function pair:
--}

newtype First a = First { first :: Maybe a }

instance Monoid (First a) where
   mempty = First Nothing
   mappend (First a) (First b) = First (a <|> b)

find :: Traversable f => (a -> Bool) -> f a -> Maybe a
find f = first . foldMap go
   where go a | f a       = First (Just a)
              | otherwise = mempty

-- Okay, great. Find the first element in the below list that is greater than 5

ellisto :: [Int]
ellisto = [1..10]

gtfo :: [Int] -> Maybe Int  -- gtfo: greather than five-o
gtfo = undefined

{--
Now, el-@yoeight defines any and all in terms of newtypes And and Or.

I happen to love And and Or, coming from Prolog ...

... some of you will get that when you're driving home from work (Andorra)

... but I think any and all can be just as well defined in terms First and
find. Don't you?

First, as First is an overlay of Maybe, we need to refine maybe as a higher-
order Bool(ean) time.

btw: it drives me crazy that the Haskell type is Bool, and not Boolean? I mean, 
like: REALLY?
--}

hoBool :: Maybe a -> Bool
hoBool (Just _) = True
hoBool Nothing  = False

{-- 
Question: is hoBool a total function? Or is it partial in that it likes only
the lower-order type, and not the higher-order one?

geddit? 'partial' and 'likes'? GEDDIT? *groan*

Now, with hoBool defined us it, First, and find to redefine any as a First/find
definition:
--}

anyAF :: Traversable f => (a -> Bool) -> f a -> Bool
anyAF = undefined

-- hint: look at the type declaration of, e.g.: hoBool . find (> 5) and anyAF (> 5)

-- Now, do similar work for all: define all in terms of hoBool, First, and find

allAF :: Traversable f => (a -> Bool) -> f a -> Bool
allAF = undefined

-- hint: is it opposite day today?

-- We'll be looking at defining and uses of Traversable types throughout this week.
