module Data.Stream where

import Control.Arrow
import Control.Comonad
import Control.Monad

infixr 5 :<

-- This is a solution to the bonus Haskell question at http://lpaste.net/107655

-- A stream is a ... well, stream of objects, usually defined by some
-- unfold-like function ('unfold' function can also be considered a kind
-- of a continuation ... but can't everything, then? Even not-continued
-- functions have a continuation: it's called 'stop.' ;)

data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
   show stream@(a :< as) = "Stream" ++ showSome 5 stream

showSome :: Show a => Int -> Stream a -> String
showSome x stream = "{ " ++ s' x stream ++ "... }"
   where s' 0 _ = ""
         s' n (a :< as) = show a ++ " :< " ++ s' (pred n) as

instance Functor Stream where
   fmap f (a :< as) = f a :< fmap f as

instance Comonad Stream where
   extract (a :< _) = a
   duplicate stream@(a :< as) = stream :< duplicate as

-- a chooser function to pick some element from the stream:

choose :: Stream a -> [(a, Stream a)]
choose (a :< as) = (a, as) : choose as

nats :: Stream Integer
nats = 0 :< (nats =>> succ . extract)

fibs :: Integral a => Stream a
fibs = 0 :< 1 :< (fibs =>> \(a :< b :< _) -> a + b)

one :: Stream Double
one = 0.5 :< (one =>> (0.5 *) . extract)

oneThird :: Stream Double
oneThird = 0.5 :< (oneThird =>> ((-0.5) *) . extract)

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (a :< as) = a : takeS (pred n) as

dropS :: Int -> Stream a -> Stream a
dropS 0 s = s
dropS n (_ :< as) = dropS (pred n) as

takeWhileS :: (a -> Bool) -> Stream a -> [a]
takeWhileS pred (a :< as) | pred a = a : takeWhileS pred as
                          | otherwise = []

dropWhileS :: (a -> Bool) -> Stream a -> Stream a
dropWhileS pred stream@(a :< as) | pred a = dropWhileS pred as
                                 | otherwise = stream

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS pred (a :< as) | pred a = a :< filterS pred as
                       | otherwise = filterS pred as

tailS :: Stream a -> Stream a
tailS (_ :< as) = as

headS :: Stream a -> a
headS (a :< _) = a
