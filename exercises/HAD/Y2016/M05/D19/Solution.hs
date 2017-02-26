{-# LANGUAGE ViewPatterns #-}

module Y2016.M05.D19.Solution where

{--
ref: http://rosalind.info/problems/orf/

Transcription May Begin Anywhere

In “Transcribing DNA into RNA”, we discussed the transcription of DNA into RNA, 
and in “Translating RNA into Protein”, we examined the translation of RNA into 
a chain of amino acids for the construction of proteins. We can view these two 
processes as a single step in which we directly translate a DNA string into a 
protein string, thus calling for a DNA codon table.

However, three immediate wrinkles of complexity arise when we try to pass 
directly from DNA to proteins. First, not all DNA will be transcribed into RNA: 
so-called junk DNA appears to have no practical purpose for cellular function. 
Second, we can begin translation at any position along a strand of RNA, meaning 
that any substring of a DNA string can serve as a template for translation, as 
long as it begins with a start codon, ends with a stop codon, and has no other 
stop codons in the middle. See Figure 1. As a result, the same RNA string can 
actually be translated in three different ways, depending on how we group 
triplets of symbols into codons. For example, ...AUGCUGAC... can be translated 
as ...AUGCUG..., ...UGCUGA..., and ...GCUGAC..., which will typically produce 
wildly different protein strings.

Problem

Either strand of a DNA double helix can serve as the coding strand for RNA 
transcription. Hence, a given DNA string implies six total reading frames, or 
ways in which the same region of DNA can be translated into amino acids: three 
reading frames result from reading the string itself, whereas three more result 
from reading its reverse complement.

An open reading frame (ORF) is one which starts from the start codon (AUG/ATG)
and ends by stop codon (TAA,TAG,TGA), without any other stop codons in between. 
Thus, a candidate protein string is derived by translating an open reading frame
into amino acids until a stop codon is reached.

Given: A DNA string s of length at most 1 kbp in FASTA format.

Return: Every distinct candidate protein string that can be translated from 
ORFs of s. Strings can be returned in any order.
--}

import Control.Monad ((>=>))
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available via 1HaskellADay git repository

import Rosalind.Types
import Rosalind.AminoAcid
import Rosalind.Bases
import Rosalind.Scan.FASTA (readFASTA)

rose99 :: String
rose99 = unlines [">Rosalind_99", "AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGAC"
                     ++ "TTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG"]

{--
Sample Output

MLLGSFRLIPKETLIQVAGSSPCNLS
M
MGMTPRLGLESLLE
MTPRLGLESLLE
--}

-- Turning a strand into a set of bases:

readbases :: IdxStrand -> [Base]
readbases = map (read . pure) . strand

allTrips :: IdxStrand -> [[NucleotideTriple]]
allTrips (GS . readbases -> b) = strands b ++ strands (reverseComplement b)

-- from there we create a set of groups starting from the start codon (ATG)
-- and ending at each end codon. Lather, rinse, repeat.

data Protein = Prot { acids :: [AcidName] } deriving (Eq, Ord)

instance Show Protein where show = map (head . show) . acids

proteins :: Set Protein -> [NucleotideTriple] -> Set Protein
proteins ans [] = ans
proteins ans (break (`elem` stops) . dropWhile (/= start) -> (p, r)) =
   if null p || null r || head r `notElem` stops then ans else
   proteins (Set.insert (Prot (map (uncode . acid codons) p)) ans) (tail p ++ r)

{--
>>> foldl proteins Set.empty (allTrips (head (readStrands (lines rose99))))
fromList [M,MGMTPRLGLESLLE,MLLGSFRLIPKETLIQVAGSSPCNLS,MTPRLGLESLLE]

or:

M
MGMTPRLGLESLLE
MLLGSFRLIPKETLIQVAGSSPCNLS
MTPRLGLESLLE
--}

sequenceDNA :: Set Protein -> [IdxStrand] -> Set Protein
sequenceDNA acc = foldl proteins acc . concatMap allTrips

orf :: FilePath -> IO ()
orf = readFASTA >=> mapM_ print . sequenceDNA Set.empty

-- and we are now sequencing DNA to extract proteins derived from those strands

-- This module is being moved to Rosalind.Proteins
