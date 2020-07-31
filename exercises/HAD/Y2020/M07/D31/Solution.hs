module Y2020.M07.D31.Exercise where

{--
Well, that's random.

Random numbers, we are told, are a hard thing. I mean, I just can't think of
the random number: 42, and get all sorts of vicious backblow about how that's
not random because of Douglas Adams.

So, we're going to tackle random numbers today.

Today, ...

... no, yesterday! we had a list of active team members. If you were to order
them from 1 to n, because this is Prolog, all of the sudden, then, first:

a) generate some random between 1 and n.
--}

import Data.Random

rnd :: Monad m => Int -> m Int
rnd hi = undefined

{--
b) Now, return 1 .. n in a random order, so:

>>> rnds 9
[8,2,7,5,1,9,3,4,6]
--}

rnds :: Monad m => Int -> m [Int]
rnds hi = undefined

-- you MAY need to replace the Monad-class with a more specific class
