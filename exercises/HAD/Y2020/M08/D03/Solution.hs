module Y2020.M08.D03.Solution where

import Y2020.M07.D30.Solution
import Y2020.M07.D31.Solution

{--
So yesterday we made some random permutations (In my opinion: I made them
with a poor implementation, but that's neither here nor there, ... today), and
the day before we had our team members iterated and classified as active and
inactive.

Today: take the active members of the team from Y2020.M07.D30.Exercise, and
with a random ordering, provided by Y2020.M07.D31.Exercise, provide a random
ordering of the active team members.

"Why?" you ask passionately. "Why, oh, why do you need a random ordering for
your active team members?" You wail into the void.

The Void responds: "Because I told you so, that's why."

It's like the Amazon interview question: "Provide an outer leaf iterator of a
binary tree." Me: "Why? Who would ever need that?" Amazon: "Why? Because I said
so."

That interview went ... 'well.'

So: moral of the story: just do whatever they say, even though you know what 
they are saying will lead to hours of work, then rework, when they realize what
idiots they were, and blame you for doing exactly what they told you to do.

Or something like that.

But, hey: their stock prices mean that they are right. So, there.

Anyway.

So the 'why,' for this exercise, is that during morning stand-up, we call on
each member in a random order, and this way I can said: "... and I wrote the
randomizer in Haskell," and mark us haskellers another time in the W-column.

ALL WE DO IZ WINNIN'!!!
--}

import Data.Map (Map)
import qualified Data.Map as Map

randomOrder :: [String] -> IO [String]
randomOrder = (\m -> rnds (Map.size m) >>= mapM (return . (m Map.!)))
            . Map.fromList
            . zip [1..] 

{--
>>> randomOrder actives 
["Jose","Ken","Robert","Tony","Morgan","Ray","Len","Howie","Victor","Doug",...]
>>> randomOrder actives 
["Tony","Robert","Doug","Morgan","Ray","Len","Jose","Victor","Howie",...]
>>> randomOrder actives 
["Nicole","Victor","Ken","Howie","Apoorv","Shoaib","Ray","Robert","Jose",...]
--}
