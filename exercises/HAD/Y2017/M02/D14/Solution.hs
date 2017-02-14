module Y2017.M02.D14.Solution where

import Control.Arrow ((&&&))

import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- import available from 1HaskellADay git repository

import Data.Percentage

{--
So, for the rest of the week, we're going to be focusing on probabilities and
probability distributions.

Okay, we have the problem of probability distributions 'solved' ...
--}

import Data.Probability

{--

or.
do.
we?

You'll note, at the end of that module, we have the probabilities of a set
of coins, one of them loaded, turning up all tails. Run that. What results
do you get?

... waiting on you ...

Okay, you see there's a probability distribution of results, the problem is
that these results (should) condense into one distribution of only two results
True and False. How do we get there?

Well, that's today's Haskell problem.

Condense a probability distribution into a set of its simplest form, and then
once condensed, return the result, itself, as a probability distribution.
--}

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

-- Using the value of flipThree (which has nothing to do with the flip function)
-- what is the condensed probability of at least one heads in a coin-toss?

atLeastOneHeads :: Prob Bool -> Percentage
atLeastOneHeads = P . fromMaybe 0 . Map.lookup False . condense'

{--
>>> atLeastOneHeads flipThree 
77.50%
--}

-- Tomorrow we'll look at what it means for a probability distribution to be
-- Applicative.
