module Y2016.M05.D19.Exercise where

-- below imports available from 1HaskellADay git repository

import Rosalind.Types
import Rosalind.AminoAcid

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
long as it begins with a start codon (AUG/ATG), ends with a stop codon (TAA,TAG,TGA), 
and has no other stop codons in the middle. As a result, the same RNA string can 
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

An open reading frame (ORF) is one which starts from the start codon and ends 
by stop codon, without any other stop codons in between. Thus, a candidate 
protein string is derived by translating an open reading frame into amino acids 
until a stop codon is reached.

Given: A DNA string s of length at most 1 kbp in FASTA format.

Return: Every distinct candidate protein string that can be translated from 
ORFs of s. Strings can be returned in any order.

Sample Dataset:
--}

rose99 :: String
rose99 = unlines [">Rosalind_99", "AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGAC"
                     ++ "TTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG"]

orf :: IdxStrand -> [[AcidName]]
orf = undefined

{--
Sample Output

MLLGSFRLIPKETLIQVAGSSPCNLS
M
MGMTPRLGLESLLE
MTPRLGLESLLE
--}
