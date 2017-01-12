module Y2017.M01.D12.Solution where

import Data.Set (Set)
import qualified Data.Set as Set

{--
Continuing the conversation of partial functions, studying Haskell pitfalls from
@bitemyapp.

Yesterday we looked at partial functions on List and solved that problem. Today,
we'll look at the more general problem of partial functions on Foldable t:

maximum and minimum.

The problem here is that these functions have nothing to go off of if your
list is empty:

*hask> maximum [1,2,3] ~> 3 works find but:
*hask> minimum []      ~> gives you
*** Exception: Prelude.minimum: empty list

Not good. But we can't write total functions just for lists because the
type-signature of these functions are:

*hask> :t maximum
maximum :: (Ord a, Foldable t) => t a -> a

So, to write total functions for maximum and minimum, we must consider any
Foldable t type, because:

*hask> minimum (Set.fromList [1,2,3,2]) ~> 1 works, but:
*hask> maximum Set.empty ~> has to work correctly, too:
*** Exception: Set.findMax: empty set has no maximal element

So, how to make this work is today's Haskell Exerise.

For maximum and minimum define total functions maximumOr and minimumOr
that take a minimum or maximum alternative value if the foldable value is
empty:
--}

minimumOr, maximumOr :: (Ord a, Foldable t) => a -> t a -> a
minimumOr minval = foldr min minval
maximumOr maxval = foldr max maxval

{--
I'll grant you:

The minimumOr and maximumOr both demand of you, the functional programmer, to
know, a priori, what your data looks like and what you expect from those data.

But, hey, if you don't know that, then, why are you coding using minimum and
maximum in the first place. "Hey, I want to take the minimum of a set of values
that I know have no values." That's dumb. Almost as dumb as: 'Hey, this possibly
empty set of values? I have no clue what the maximum and minimum values may be,
so I'll just assume 0 is the minimum, because I have no clue if there are 
negative values here."

Okay, fer realz? fo' shur? because ... REALLY?

But I digress.

So, what are the values of maximumOr 0 and minimumOr 314159 for the following
values:
--}

nada, soma :: [Int]
nada = []
soma = [1..99]

{--
*Y2017.M01.D12.Solution> [minimumOr 314159, maximumOr 0] <*> [nada,soma] ~>
[314159,1,0,99]
--}

zumbo, jumbo :: Set Int
zumbo = Set.empty
jumbo = Set.fromList [236..1040]

{--
*Y2017.M01.D12.Solution> [minimumOr 314159, maximumOr 0] <*> [zumbo,jumbo] ~>
[314159,236,0,1040]
--}

-- There's a Haskell exercise on the beauty of Control.Applicative in here somewhere...
