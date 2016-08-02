module Y2016.M08.D03.Exercise where

import Y2016.M08.D01.Exercise (summer)

{--
This exercise is called 'cheat sheet.'

So in the function imported above, summer, it gives you all permutations of
the solutions when you are summing to a number using x slots. Great!

Super, in fact, if you eventually build a system that combines all these sums
and slots to solve a kakuro puzzle for you.

http://www.menneske.no/kakuro/eng/

But let's say you're not quite there yet, because, well, you are not, and, still
solving these puzzles by hand, you need to know: hm, what are just the digits 
that sum to, say, 27, and I have four slots. Since order is not important, I 
don't need to see all the possible permutations.

What we need is the function summer without permute at the end.

What we HAVE is summer WITH permute at the end.

How do I see this:

cheatSheet 27 4 ~> [[3,7,8,9],[4,6,8,9],[5,6,7,9]]

given summer 27 4 gives me this as its value:

[[3,7,8,9],[3,7,9,8],[3,8,7,9],[3,8,9,7],... 72 values in the returned list
--}

cheatSheet :: Int -> Int -> [[Int]]
cheatSheet total nslots = undefined

-- hint: use summer to help you find cheatSheet
