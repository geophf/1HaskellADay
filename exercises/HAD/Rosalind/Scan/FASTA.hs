module Rosalind.Scan.FASTA where

{--
The FASTA-file format is as follows:

>Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC

... etc.

We need to read in strands of sequenced DNA and tag them.
--}

-- below import available via 1HaskellADay git repository

import Rosalind.Types

readFASTA :: FilePath -> IO [IdxStrand]
readFASTA = fmap (readStrands . lines) . readFile

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
