module Y2017.M02.D27.Exercise where

import Data.Array
import Data.List (subsequences)
import Data.Ratio (numerator)

-- below imports available via 1HaskellADay git repository

import Analytics.Math.Combinatorics (choose)
import Control.List (takeout)

{--
So, I'm eyeing http://rosalind.info/problems/lcsq/ which is a problem to find
the largest common subsequence of two 'strings' (lists of n elements). The
problem is not the problem, however, the problem is that Data.List.subsequences
has 

1) the combinatorial-explosion problem:

>>> subsequences [1,2,3]
[[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

and 2) I'm looking for longest first, not last.

If only there were a function where I could throttle the results returned by
subsequences and give longest firstest, or, in other words: gives subsequences
of length x in a controlled fashion.

Today's Haskell problem.

Given [a] and a length n (where we hope n is less than list length) return all
subsequences of length n of [a].
--}

subseq :: Int -> [a] -> [[a]]
subseq n as = undefined

-- hint: one, or more, of the above imports MAY be helpful here, also you may 
-- wish to constrain the type of a? Not sure about that, however.

-- What are the subsequences of length 2 for the list [1,2,3]?
-- What are the subsequences of length 6 of the String "AACCTTGG"?

{-- BONUS -----------------------------------------------------------------

A silver bullet?

So, now that we have a 'faster' subsequencing algorithm, can we just waltz to
solving the rosalind-problem?

Hm.

Fake question: what are the subsequences of (subseq 49 [1..75])?
(don't do this at home, kids!)

Unfake question, or, put another way: what is the value of 75 `choose` 49?
--}

alotta :: Integer
alotta = numerator (75 `choose` 49) -- we ... 'know' the demoninator is 1
                                    
-- A thought-experiment: how do we guarantee (demoninator == 1) in the types?
