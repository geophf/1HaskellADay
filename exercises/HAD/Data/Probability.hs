{-# LANGUAGE TupleSections #-}

module Data.Probability where

import Control.Arrow (first, second, (***), (&&&))

import Data.Function (on)
import Data.List (groupBy, sort, genericLength)
import Data.Map (Map)
import qualified Data.Map as Map
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
   Prob fs <*> Prob xs = Prob (map (\(f,p) -> f *** (p *)) fs <*> xs)

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

-- Exercise, return Probabilities as a mapping. We do this with condense':

condense' :: Ord a => Prob a -> Map a Rational
condense' = Map.fromList . map (fst . head &&& foldr (+) 0 . map snd)
          . groupBy ((==) `on` fst) . sort . getProb

{--
>>> flipThree
Prob {getProb = [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),
                 (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]}
>>> condense' flipThree
fromList [(False,31 % 40),(True,9 % 40)]
--}

-- now that you've got the condensate, recoalesce the result into a probability
-- distribution

recoalesce :: Ord a => Map a Rational -> Prob a
recoalesce = Prob . Map.toList

{--
>>> recoalesce $ condense' flipThree
Prob {getProb = [(False,31 % 40),(True,9 % 40)]}
--}

-- let's tie everything together into one function:

condense :: Ord a => Prob a -> Prob a
condense = recoalesce . condense'

-- we also need to represent a uniform probability distribution, that is: for any
-- value x in a probability distribution, any value occurs with equal likelihood

uniform :: [a] -> Prob a
uniform as = let prob = 1 % genericLength as in Prob (map (,prob) as)
