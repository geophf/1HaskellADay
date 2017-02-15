module Rosalind.Scan.FASTA where

import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio

-- below imports available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin, (-|))
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.Percentage

{--
Computing GC Content, a solution to the problem posted at
http://lpaste.net/116305

We intermingle a bit here: a bit of scanning, a bit of analysis, but
if we want to read in a FASTA file, then readFASTA file

will get you what you need.
--}

type DNAStrand = String

gcContent :: DNAStrand -> Percentage
gcContent strand =
   let gcs = getSum (sumGC (Map.toList (Bag.fromList strand)))
   in  P (uncurry (%) $ adjoin fromIntegral (gcs, length strand))

sumGC :: Monoid m => [(Char, m)] -> m
sumGC = foldr (\(x,y) -> ((x `elem` "GC" -| y) <>)) mempty
-- sumGC ((x, y):rest) = (if x `elem` "GC" then y else 0) + sumGC rest

type Ident = String

data IdxStrand = IS { ident :: Ident, strand :: DNAStrand }
   deriving (Eq, Ord, Show)

readFASTA :: FilePath -> IO [IdxStrand]
readFASTA = fmap (readStrands . words) . readFile

-- Your basic builder-function, building a FASTA strand:
-- First line is the FASTA ID
-- Following lines are the sequenced DNA

readIdxStrand :: [String] -> [(IdxStrand, [String])]
readIdxStrand [] = []
readIdxStrand (('>':rosy):rest) = return (IS rosy strand, remaining)
   where (strand, remaining) = grabStrand rest []
         grabStrand [] str = (str, [])
         grabStrand ans@(('>':_):_) str = (str, ans)
         grabStrand (x:y) str = grabStrand y (str ++ x)

{--
*Main> liftM (readIdxStrand . lines) (readFile "samp_strands_gc.txt" )
[(IS "Rosalind_6404" ("CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGC"
                      ++ "TTCCGGCCTTCCCTCCCACTAATAATTCTGAGG"),
    [">Rosalind_5959","CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCT"
                  ++ "CCGCCGAAGGTCT", "ATATCCATTTGTCAGCAGACACGC",
     ">Rosalind_0808","CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTT"
                  ++ "CAGACCAGCCCGGAC", "TGGGAACCTGCGGGCAGTAGGTGGAAT"])]
--}

readStrands :: [String] -> [IdxStrand]
readStrands = parseStrands . readIdxStrand

parseStrands :: [(IdxStrand, [String])] -> [IdxStrand]
parseStrands [] = []
parseStrands ((strand, rest):_) = strand : readStrands rest

{--
*Main> readFASTA "Rosalind/rosy_strands.txt"
[IS "Rosalind_6404" ("CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTT"
                     ++ "CCGGCCTTCCCTCCCACTAATAATTCTGAGG"),
 IS "Rosalind_5959" ("CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCG"
                     ++ "CCGAAGGTCTATATCCATTTGTCAGCAGACACGC"),
 IS "Rosalind_0808" ("CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACC"
                     ++ "AGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT")]
--}

is2gc :: IdxStrand -> (Ident, Percentage)
is2gc (IS name strand) = (name, gcContent strand)

maxGcContent :: [IdxStrand] -> (Ident, Percentage)
maxGcContent = maximumBy (compare `on` snd) . map is2gc

{--
*Main> liftM (maxGcContent . readStrands . lines) (readFile "rosy_strands.txt")
("Rosalind_0808",60.91954022988506)
*Main> liftM (maxGcContent . readStrands . lines) (readFile "rosalind_gc.txt")
("Rosalind_5105",52.22222222222222)

WOOT! I say!
--}
