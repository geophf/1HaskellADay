module Y2017.M02.D27.Solution where

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

-- hint: one, or more, of the above imports MAY be helpful here, also you may 
-- wish to constrain the type of a? Not sure about that, however.

-- nah. If we put [a] into an array, we can then choose on the indices, or
-- we can just drop the unwanted elements and skip the whole array-translation
-- thing

subseq n = flip (ssq n) [] . length <*> id

ssq :: Int -> Int -> [a] -> [a] -> [[a]]
ssq 0 _ ans _            = return (reverse ans)
ssq n m acc list | n > m = []   -- no more elements to draw from, so we fail out
                 | n < 0 = []   -- and this is just weird, so quit
                 | otherwise = [0 .. m]                >>= \d -> 
                               shift acc (drop d list) >>=
                               uncurry (ssq (pred n) (m - d - 1))

shift :: [a] -> [a] -> [([a], [a])]
shift acc (h:t) = return (h:acc, t)
shift acc []    = []  -- no shift occurred, so we fail out.

{--
What are the subsequences of length 2 for the list [1,2,3]?

>>> subseq 2 [1,2,3]
[[1,2],[1,3],[2,3]]

What are the subsequences of length 6 of the String "AACCTTGG"?

>>> subseq 6 "AACCTTGG"
["AACCTT","AACCTG","AACCTG","AACCTG","AACCTG","AACCGG","AACTTG","AACTTG",
 "AACTGG","AACTGG","AACTTG","AACTTG","AACTGG","AACTGG","AATTGG","ACCTTG",
 "ACCTTG","ACCTGG","ACCTGG","ACTTGG","ACTTGG","ACCTTG","ACCTTG","ACCTGG",
 "ACCTGG","ACTTGG","ACTTGG","CCTTGG"]

>>> length (subseq 6 "AACCTTGG")
28

-- Note the redundancies!

>>> Set.fromList (subseq 6 "AACCTTGG")
fromList ["AACCGG","AACCTG","AACCTT","AACTGG","AACTTG","AATTGG","ACCTGG",
          "ACCTTG","ACTTGG","CCTTGG"]

>> length (Set.fromList (subseq 6 "AACCTTGG"))
10
--}

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

{-- 
>>> alotta
101131821425286333900

Hm. So just making a more effient subseq algorithm doesn't make the 'bigness' of
the rosalind-problem go away. We'll look at another approach tomorrow.
--}
