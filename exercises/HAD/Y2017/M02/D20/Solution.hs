module Y2017.M02.D20.Solution where

import Control.Arrow ((&&&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import qualified Data.Bag as Bag
import Data.Percentage

import Rosalind.Types
import Rosalind.GCContent
import Rosalind.Scan.FASTA

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
gcContent' dna =

-- our nucleotide-totals is as follows:

   let nukes = Map.toList (Bag.fromList dna)

-- well, the totals of all the nucleotides IS the length of the list:

       len   = sum (map snd nukes)

-- the length of nukes is 4 or less, dna can have thousands of nucleotides

-- And now, along with the old definition, we have everything we need

   in  P (uncurry (%) $ adjoin (fromIntegral . getSum) (sumGC nukes, len))

-- Now, using Rosalind/rosy_strands.txt verify that gcContent' == gcContent

{--
>>> fmap (map (ident &&& gcContent . strand)) $ readFASTA "Rosalind/rosy_strands.txt" 
[("Rosalind_6404",53.75%),("Rosalind_5959",53.57%),("Rosalind_0808",60.91%)]
>>> fmap (map (ident &&& gcContent' . strand)) $ readFASTA "Rosalind/rosy_strands.txt" 
[("Rosalind_6404",53.75%),("Rosalind_5959",53.57%),("Rosalind_0808",60.91%)]
--}

-- moving gcContent' definition to Rosalind.GCContent.gcContent
