module Data.Probability where

import Control.Arrow (first, second)
import Data.Ratio

-- A direct lift from http://learnyouahaskell.com/for-a-few-monads-more#making-monads

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
  fmap f = Prob . map (first f) . getProb

{--
Data.Probability> fmap negate (Prob [(3,0.5), (5, 0.25), (9,0.25)])
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}
--}

flatten :: Prob (Prob a) -> Prob a
flatten = Prob . concatMap multAll . getProb
   where multAll = map . (second . (*) . snd) <*> getProb . fst

-- from flatten (which is really the signature for join) we build Monad Prob

instance Monad Prob where
   return x = Prob [(x,1)]
   m >>= f  = flatten (fmap f m)
   fail _   = Prob []       -- the mempty case

-- Of course, now-a-days, we need to make the monad applicative, because reasons

instance Applicative Prob where
   pure x = Prob [(x,1)]
   f <*> prob = undefined -- I have no idea

-- From the above, with a loaded coin, we can compute the probabilities of
-- all tails in a coin-toss:

data Coin = Heads | Tails deriving (Eq, Ord, Show)

coin, loadedCoin :: Prob Coin
coin = Prob [(Heads, 0.5), (Tails, 0.5)]
loadedCoin = Prob [(Heads, 0.1), (Tails, 0.9)]

flipThree :: Prob Bool
flipThree = coin >>= \a -> coin >>= \b -> loadedCoin >>= \c ->
            return (all (==Tails) [a,b,c])

-- Should be what?

-- Exercise, return Probabilities as a mapping
