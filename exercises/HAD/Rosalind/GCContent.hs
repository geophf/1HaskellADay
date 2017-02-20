module Rosalind.GCContent where

{--
Computing GC Content
Aug. 2, 2012, 8 p.m. by Rosalind Team Topics: String Algorithms

Identifying Unknown DNA Quickly

Problem

The GC-content of a DNA string is given by the percentage of symbols 
in the string that are 'C' or 'G'. For example, the GC-content of 
"AGCTATAG" is 37.5%. Note that the reverse complement of any DNA string 
has the same GC-content.

DNA strings must be labeled when they are consolidated into a database. 
A commonly used method of string labeling is called FASTA format. In this 
format, the string is introduced by a line that begins with '>', followed 
by some labeling information. Subsequent lines contain the string itself; 
the first line to begin with '>' indicates the label of the next string.

In Rosalind's implementation, a string in FASTA format will be labeled by 
the ID "Rosalind_xxxx", where "xxxx" denotes a four-digit code between 
0000 and 9999.

Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).

Return: The ID of the string having the highest GC-content, followed by the 
GC-content of that string. Rosalind allows for a default error of 0.001 in 
all decimal answers unless otherwise stated; please see the note on absolute 
error below.

Sample Dataset

>Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT

Sample Output

Rosalind_0808
60.919540
--}

import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin, (-|))
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.Percentage

import Rosalind.Types

gcContent :: DNAStrand -> Percentage
gcContent strand =
   let gcs = getSum (sumGC (Map.toList (Bag.fromList strand)))
   in  P (uncurry (%) $ adjoin fromIntegral (gcs, length strand))

sumGC :: Monoid m => [(Char, m)] -> m
sumGC = foldr (\(x,y) -> ((x `elem` "GC" -| y) <>)) mempty
-- sumGC ((x, y):rest) = (if x `elem` "GC" then y else 0) + sumGC rest

is2gc :: IdxStrand -> (Ident, Percentage)
is2gc (IS name strand) = (name, gcContent strand)

maxGcContent :: [IdxStrand] -> (Ident, Percentage)
maxGcContent = maximumBy (compare `on` snd) . map is2gc
   -- maximumBy (\idx1 idx2 -> compare (snd idx1) (snd idx2)) . map is2gc

{--
import Rosalind.Scan.FASTA

>>> fmap maxGcContent (readFASTA "Rosalind/rosy_strands.txt")
("Rosalind_0808",60.91%)

WOOT! I say!
--}
