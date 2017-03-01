module Y2017.M03.D01.Exercise where

import Control.Monad.State
import Data.Time.Clock

-- below imports available via 1HaskellADay git repository

import Data.FiniteList
import Data.Random
import Rosalind.Bases
import Rosalind.Types (DNAStrand)

{--
So, yesterday, we saw that a more effient subseq does not create the silver
bullet, as the combinations of subsequences for even small lists is just not
computationally feasible ... I mean, for conventional computers or for quantum
computers with only a few qubits.

So that's a problem. That's today's Haskell problem.

Given two lists, as and bs find a common subsequence (largest subsequence first)
... efficiently.

Oy veh.
--}

largestCommonSubseq :: Eq a => [a] -> [a] -> Maybe [a]
largestCommonSubseq as bs = undefined

-- of course, as == bs then Just as is the answer. Of course!
-- but the reductive cases from there? That can get tricky. Maybe we want
-- to keep track of the length of the lists we're working with?

-- 'Maybe,' indeed.

-- then, with these vectors, let's find the common subsequences starting with
-- the uncurry min (adjoin length (vect1, vect2)) down to no commonality:

commonSubseq :: Eq a => Vect a -> Vect a -> Int -> [[a]]
commonSubseq vas vbs n = undefined

{--
So, then: largestCommonSubseq definition defers to informed applications of
commonSubseq where the first result is the 'Just' result, and no result from
any application of commonSubseq gets you 'Nothing.'

Hint: we know that constructing all the subsequences of length n of as first is
computationally infeasible, so ... perhaps:

1. select element in as, call it a.
2. scan forward into bs for a, returning restbs
3. if length restbs < n then fail else decrement n and loop

You'll find the common subsequences of length n that way, but is it efficient?

Let's see.

Find the longest common subsequence (that is: the first common subsequence, 
longest first) of the following:
--}

rosalind23, rosalind64 :: DNAStrand
rosalind23 = "AACCTTGG"
rosalind64 = "ACACTGTGA"

-- your answer should be:

result :: DNAStrand
result = "AACTGG"

-- note: rosalind23 and rosalind64 are not the same length!

{-- BONUS -----------------------------------------------------------------

Generate pairs of random DNAStrands of length n where n <- [15, 25, 35, 50, 100]
(recall, although this is not formalized in the type system, that a DNAStrand
has members <- "ACGT")

Find the longest common subsequences of these pairs. Do you get bogged down
somewhere? If so, why?
--}

rndStrand :: Monad m => Int -> StateT (RNG Int) m DNAStrand
rndStrand n = undefined

-- okay, now with your generated strands, time how long it takes to find the
-- longest common subsequence:

go :: Int -> IO (Maybe DNAStrand, NominalDiffTime)
go strandLength = undefined

-- Longest common subsequence of two random DNAStrands of length 15 is:

-- ... of length 25?

-- ... of length 35?

-- ... of length 50?

-- ... of length 100?
