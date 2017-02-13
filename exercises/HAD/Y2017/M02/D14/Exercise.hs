module Y2017.M02.D14.Exercise where

import Data.Map (Map)

{--
So, for the rest of the week, we're going to be focusing on probabilities and
probability distributions.

Okay, we have the problem of probability distributions 'solved' ...
--}

import Data.Percentage
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
condense' prob = undefined

-- now that you've got the condensate, recoalesce the result into a probability
-- distribution

recoalesce :: Ord a => Map a Rational -> Prob a
recoalesce values = undefined

-- let's tie everything together into one function:

condense :: Ord a => Prob a -> Prob a
condense prob = undefined

-- Using the value of flipThree (which has nothing to do with the flip function)
-- what is the condensed probability of at least one heads in a coin-toss?

atLeastOneHeads :: Prob Bool -> Percentage
atLeastOneHeads cointoss = undefined

-- Tomorrow we'll look at what it means for a probability distribution to be
-- Applicative.
