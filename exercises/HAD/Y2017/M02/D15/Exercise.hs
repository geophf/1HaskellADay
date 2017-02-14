module Y2017.M02.D15.Exercise where

import Data.Probability

import Y2017.M02.D14.Exercise (condense)

{--
What does it mean for a probability distribution to be Applicative? Well, we
already see what it means for a probability distribution to be monadic: one
simply distributes the function across the values at the probability, if you
have multiple distribution, those probabilities are condensed (summed) with
the recoalesced result right there for you. Yay!

So, we see monadic application is summing.

What's applicative?

Well, let's think this through.

If you have a probability distribution of functions, and you wish to apply
them to a probability distribution of values, what is the resulting probability
distribution?

Well, let's look at the function types for Applicative Functor application:

(<*>) :: Applicative f => f (a -> b) -> f a -> f b

That means applying the functions (at probabilities p1) to the values (a) at
probabilities p2, gets you a probability distribution of (f a) values at
probabilities ... what?

You actually don't need to know probability theory to know the answer (if you
do know, great), what does type theory say?

Function application is ... additive? Nope.

It's multiplicative.

That's a hint right there for ya.

Today's #Haskell problem. Write Prob as an Applicative instance, or, since
we have (<*>) defined as undefined in Data.Probabilities, define an application
function for probability distributions.
--}

apply :: Prob (a -> b) -> Prob a -> Prob b
apply (Prob fs) (Prob as) = undefined

-- make sure the applicative functor laws hold for the above definition

-- Now given the probability distribution of functions:

pfs :: (Enum a, Num a) => Prob (a -> a)
pfs = Prob [(succ, 0.5), (pred, 0.25), ((* 2), 0.25)]

-- And a set of probabilities of a value falling into a set of bins:

pns :: Prob Int
pns = uniform [1..9]

-- What is the (coalesced) set of probabilities of the new bins?

-- oh, you have to write the uniform distribution function, that is: for any
-- value x in a probability distribution, any value occurs with equal likelihood

uniform :: [a] -> Prob a
uniform as = undefined

-- what is the (coalesced/condensed) value (apply pfs pns)?

-- apply and condense will be rolled into the Data.Probabilities module.
