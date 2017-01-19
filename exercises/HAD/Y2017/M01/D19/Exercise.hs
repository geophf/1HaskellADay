module Y2017.M01.D19.Exercise where

-- below imports available via 1HaskellADay git repository

import Y2017.M01.D16.Exercise

{--
And lastly from @Jose_A_Alonso @yoeight, lastest, in the theme of Traversable.

So we've seen the First / find pairing (see import above).

But how do we get the 'last' element of a Traversable type? What does that
even mean?

Today's Haskell exercise: define lastest.
--}

lastest :: Traversable t => t a -> Maybe a
lastest = undefined

-- Question: Is lastest a total function? Let's see:

nada, uno, multa :: [Int]
nada = []
uno = [1]
multa = [1..10]

-- What is the lastest for each of these Traversables: nada, uno, multa?

-- Tomorrow, we'll look at making Merkle trees Traversable and see what
-- that means for these functions we've defined.
