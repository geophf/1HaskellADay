module Y2017.M02.D20.Exercise where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

-- below imports available via 1HaskellADay git repository

import Data.Bag
import Data.Percentage

import Rosalind.Types
import Rosalind.GCContent

{--
Is there a better way?

In above Rosalind.GCContent import we have this function:

gcContent :: DNAStrand -> Percentage
gcContent strand =
   let gcs = getSum (sumGC (Map.toList (Bag.fromList strand)))
   in  P (uncurry (%) $ adjoin fromIntegral (gcs, length strand))

The thing is, okay, it works, but how does it work?

1. It iterates through the DNA string to get the GC-content
2. It iterates through the DNA string, again, to get the length.

Whenever I see the length-function I have a little annoying voice saying:

You could do better.

Some cases I do have to call the length function, but in some cases, I do not.

This is one of those cases.

rewrite gcContent so that the DNA string is iterated only once. That is to say,
somewhere (else) the length of this list (String) is already recorded. Find that
record and use it to compute the GC-content signature of a string.
--}

gcContent' :: DNAStrand -> Percentage
gcContent' dna = undefined

-- Now, using Rosalind/rosy_strands.txt verify that gcContent' == gcContent
