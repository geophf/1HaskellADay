module Y2017.M01.D19.Solution where

import Data.Monoid hiding (First)

-- below imports available via 1HaskellADay git repository

import Y2017.M01.D16.Solution

{--
And lastly from @Jose_A_Alonso @yoeight, lastest, in the theme of Traversable.

So we've seen the First / find pairing (see import above).

But how do we get the 'last' element of a Traversable type? What does that
even mean?

Today's Haskell exercise: define lastest.
--}

-- so, from @yoeight we have First, let's do the Dual of that ... which is 
-- part of Data.Monoid

{--
*Y2017.M01.D19.Solution> Dual (First (Just 'X')) <> Dual First (Const (Just 'Y'))
Dual {getDual = Const {val = Just 'Y'}}
--}

lastest :: (Traversable t) => t a -> Maybe a
lastest = first . getDual . foldMap (Dual . First . Just)

-- Question: Is lastest a total function? Yes. Let's see:

nada, uno, multa :: [Int]
nada = []
uno = [1]
multa = [1..10]

{-- 
What is the lastest for each of these Traversables: nada, uno, multa?

*Y2017.M01.D19.Solution> map lastest [nada, uno, multa]
[Nothing,Just 1,Just 10]
--}

-- Tomorrow, we'll look at making Merkle trees Traversable and see what
-- that means for these functions we've defined.
