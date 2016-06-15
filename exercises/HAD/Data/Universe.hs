module Data.Universe where

{--
The Universe data type (not from logic) of universal extent to the left 
and right; perfect for cellular automata http://lpaste.net/108632

So, fun fact: the fifty-zillion best seller "50 Shades of Grey(er)(est)"
was originally Twilight fan-fiction (submissive Bella meets smoky-eyed
Dom Edward, in case you missed the obvious parallels) written under the
pen name of, and I'm not joking here, 'Master of the Universe.'

I mention this in passing.

So. An universe is a type of universal extent to the left and to the right:
--}

import Control.Comonad
import Control.Arrow ((&&&))

data U a = U { past :: [a], present :: a, puture :: [a] }

right :: U a -> U a
right (U a b []) = U a b []
right (U a b (c:cs)) = U (b:a) c cs

left :: U a -> U a
left (U [] b c) = U [] b c
left (U (a:as) b c) = U as a (b:c)

instance Functor U where
   fmap f (U a b c) = U (map f a) (f b) (map f c)

instance Comonad U where
   extract (U _ b _) = b
   duplicate a = U (tail $ iterate left a) a (tail $ iterate right a)

shift :: Int -> U a -> U a
shift i u = iterate (if i < 0 then left else right) u !! abs i

-- of course, if Universes were Foldable ...

toList :: Int -> Int -> U a -> [a]
toList i j u = take (j-i) $ half $ shift i u 
   where half (U _ b c) = b:c

instance Show a => Show (U a) where
   show univ = "U " ++ show (toList (-10) 10 univ)

showCompact :: Show a => U a -> Int -> String
showCompact univ haWidth =
   "U " ++ concatMap show (toList (negate haWidth) (2 * haWidth) univ)

{--
Debt of gratitude to sigfpe for the type from his article on Comonadic
cellular automata. 

http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

This type is used for, e.g. modelling cellular automata, particularly
the rules for generation of the same. See: http://lpaste.net/107206

And there you have it! An Universe, just for you! So don't tell me I 
never did anything for you, eh?
--}

looking :: (U a -> [a]) -> U a -> [a]
looking there = uncurry (:) . (present &&& there)

forward, back :: U a -> [a]
forward = puture
back = past
