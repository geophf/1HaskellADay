{-# LANGUAGE TupleSections #-}

module Y2017.M03.D01.Solution where

import Control.Monad (liftM2)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock

-- below imports available via 1HaskellADay git repository

import Analytics.Math.Combinatorics (choose)
import Control.Logic.Frege (adjoin)
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
largestCommonSubseq as bs | as == bs  = Just as
                          | otherwise = 
   let vects = adjoin list2Vect (as, bs)
       start = uncurry min (adjoin len vects) in

-- first attempt ... but are we generating everything here?

   case (concatMap (uncurry commonSubseq vects) (reverse [0 .. start])) of
      []    -> Nothing  -- this branch ... 'may' ... take a while ...
      (h:_) -> Just h   -- I hope this branch occurs 'sooner.'

-- next attempt: tomorrow

{-- 
Of course, as == bs then Just as is the answer. Of course!

>>> largestCommonSubseq "abc" "abc"
Just "abc"

but the reductive cases from there? That can get tricky. Maybe we want to keep 
track of the length of the lists we're working with?

'Maybe,' indeed.

Then, with these vectors, let's find the common subsequences starting with the 
uncurry min (adjoin length (vect1, vect2)) down to no commonality:
--}

commonSubseq :: Eq a => Vect a -> Vect a -> Int -> [[a]]
commonSubseq = comssq []

comssq :: Eq a => [a] -> Vect a -> Vect a -> Int -> [[a]]
comssq accum as bs n | n < 0 || n > len as || n > len bs = []
                     | n == 0                            = return (reverse accum)
                     | otherwise                         =

-- first we want to choose an arbitrary element from as

   [0.. n] >>= \d ->
   fromMaybe [] (splitV (dropV d as) >>= \(h,t) ->

-- then we locate that same element in bs, then drop that element from bs:

                 splitV (dropUntilV (== h) bs) >>= \(_, n00bs) ->

-- geddit? new bs, n00bs? GEDDIT?

-- Okay, with the new bs (n00bs) we continue our search:

                 return (comssq (h:accum) t n00bs (pred n)))

{--
Find the longest common subsequence (that is: the first common subsequence, 
longest first) of the following:
--}

rosalind23, rosalind64 :: DNAStrand
rosalind23 = "AACCTTGG"
rosalind64 = "ACACTGTGA"

-- your answer should be in:

result :: Set DNAStrand
result = Set.fromList ["AACTGG","AACTTG","ACCTGG","ACCTTG"]

-- note: rosalind23 and rosalind64 are not the same length!

{--
>>> largestCommonSubseq rosalind23 rosalind64
Just "AACTTG"

>>> fmap (`elem` result) it
Just True

It's JUST ... SO ... TRUE!

(Haskell said it, so it MUST be TRUE!)
--}

{-- BONUS -----------------------------------------------------------------

Generate pairs of random DNAStrands of length n where n <- [15, 25, 35, 50, 100]
(recall, although this is not formalized in the type system, that a DNAStrand
has members <- "ACGT")

Find the longest common subsequences of these pairs. Do you get bogged down
somewhere? If so, why?
--}

rndStrand :: Monad m => Int -> StateT (RNG Int) m DNAStrand
rndStrand = fmap show . rndGeneSequence

go :: Int -> IO (Maybe DNAStrand, NominalDiffTime)
go n = rndSeed        >>= \seed ->
       getCurrentTime >>= \start ->
       evalStateT (liftM2 largestCommonSubseq (rndStrand n) (rndStrand n)) seed >>=
       \ans -> print ans >>
       getCurrentTime >>= return . (ans,) . flip diffUTCTime start

{--
Longest common subsequence of two random DNAStrands of length 15 is:

>>> go 15
(Just "TATCCATTAG",0.052205s)

... in no time

... of length 25? (Just "ACGATGAATCACCA",1.426311s) ... uh, oh!

-- ... of length 35? (Just "TTGAGGGGCGGGGTTTGACGAT",64.026918s)

... are we generating all the solutions? I think not but ...

>>> sum (map (choose 35) [1 .. 35])
34359738367 % 1

but that's inaccurate, as we didn't hit every combination: we stopped at the
strand above which is 22 nucleotides, so we actually only touched approximately:

>>> sum (map (choose 35) [23 .. 35])
1538132224 % 1

So it's possible we've iterated through every one. Let's jigger the definition.
(but tomorrow)

-- ... of length 50?

Nope, I got bored waiting here. We need to redesign. On it!

-- ... of length 100? Nope, nope, nopy-nope!
--}
