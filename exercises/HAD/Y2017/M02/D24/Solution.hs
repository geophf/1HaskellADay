module Y2017.M02.D24.Solution where

-- below imports from cabal package 'statistics'

import Statistics.Distribution
import Statistics.Distribution.Binomial

{--
AHA!

Whenever you think you have to invent something, stop, pause, and think, ...

SOMEBODY ELSE HAS ALREADY DONE IT! USE THE DARK SIDE OF THE FORCE AND TOGETHER
WE CAN RULE THE GALAXY AS FATHER AND SON!

See? All you need is love, a psychopathic father that murders young children
at the Jedi Academy, and 'midichlorians.' Lots of 'midichlorians.'

Where was I?

Ah, yes: 'already invented.'

So, we know from Rosalind.Traits that any descendant from an Aa Bb/Aa Bb
pairing will have a 25% probability of itself being Aa Bb (you can verify
this if you'd like), so the question arises, what is the probability of at
least n offspring at generation x (or Generation X, if you prefer) also being
Aa Bb.

This is the cumulative (or actually co-cumulative) binomial probability
distribution.

So.

Today's Haskell problem: with the help of the above imports, create a binomial
probability distribution at 25%, run a scenario where you are in generation
x, where the xth generation has x^2 offspring, and you are looking for at least
n of the offspring to have the desired characteristic (in the abstract case,
the what the characteristic is doesn't matter, we need what the probably outcome
of having it.
--}
 
type Trials = Int

aAbB :: Trials -> BinomialDistribution
aAbB = (`binomial` 0.25)

genx :: Int -> BinomialDistribution -> Double
genx n bin = sum (map (probability bin) [n .. bdTrials bin])

{-- 
What is the result for gen = 2, n = at least 1?

>>> genx 1 (aAbB 4)
0.68359375

Now solve for gen = 7, n = at least 35:

>>> genx 35 (aAbB (2 ^ 7))
0.3006022057910504

*WHEW* That was a lot easier than rewriting probability theory by hand!
--}
