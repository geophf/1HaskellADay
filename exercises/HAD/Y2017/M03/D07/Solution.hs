module Y2017.M03.D07.Solution where

import Control.Arrow ((&&&))
import Data.List (sort, group)
import Data.Maybe (catMaybes)

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import Control.Presentation (laxmi)

import Rosalind.Bases
import Rosalind.Types
import Rosalind.Scan.FASTA

{--
url: http://rosalind.info/problems/tran/

Transitions and Transversions solved by 2250 as of March 7th, 2017

Certain Point Mutations are More Common

Problem

For DNA strings s1 and s2 having the same length, their transition/transversion 
ratio R(s1,s2) is the ratio of the total number of transitions to the total 
number of transversions, where symbol substitutions are inferred from mismatched
corresponding symbols as when calculating Hamming distance (see “Counting Point 
Mutations”).

Figure 1. Illustration of transitions and transversions.

Point mutations occurring in DNA can be divided into two types: transitions and 
transversions. A transition substitutes one purine for another (A↔G) or one 
pyrimidine for another (C↔T); that is, a transition does not change the 
structure of the nucleobase. Conversely, a transversion is the interchange of a 
purine for a pyrimidine base, or vice-versa. See Figure 1. Transitions and 
transversions can be defined analogously for RNA mutations.

Because transversions require a more drastic change to the base's chemical 
structure, they are less common than transitions. Across the entire genome, the 
ratio of transitions to transversions is on average about 2. However, in coding 
regions, this ratio is typically higher (often exceeding 3) because a transition
appearing in coding regions happens to be less likely to change the encoded 
amino acid, particularly when the substituted base is the third member of a 
codon (feel free to verify this fact using the DNA codon table). Such a 
substitution, in which the organism's protein makeup is unaffected, is known as 
a silent substitution.

Because of its potential for identifying coding DNA, the ratio of transitions to
transversions between two strands of DNA offers a quick and useful statistic for
analyzing genomes.

Given: Two DNA strings s1 and s2 of equal length (at most 1 kbp).

Return: The transition/transversion ratio R(s1,s2)
--}

sample :: String
sample = unlines [">Rosalind_0209",
                  "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGA",
                  "AGTACGGGCATCAACCCAGTT",">Rosalind_2200",
                  "TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGC",
                  "GGTACGAGTGTTCCTTTGGGT"]

result :: String
result = "1.21428571429"

transTrans :: (GeneSequence, GeneSequence) -> Double
transTrans = uncurry (/) . adjoin (fromIntegral . length) . (head &&& last) 
           . group . sort . catMaybes
           . uncurry (zipWith (inequalAnd isTransversion)) . adjoin bases

roundResult :: Double -> String
roundResult = laxmi 11 . toRational . (+ 0.000000000005)

inequalAnd :: Eq a => (a -> a -> Bool) -> a -> a -> Maybe Bool
inequalAnd f a b | a == b    = Nothing
                 | otherwise = return (f a b)

isTransversion :: Base -> Base -> Bool
isTransversion base = if isPurine base then isPyrimidine else isPurine

isPurine :: Base -> Bool
isPurine ag = ag == A || ag == G

isPyrimidine :: Base -> Bool
isPyrimidine ct = ct == C || ct == T

{--
>>> transTrans . adjoin (read . strand) . (head &&& last) $ readStrands (lines sample) 
1.2142857142857142
>>> roundResult it
"1.21428571429"
>>> it == result 
True
--}

-- And let's pull it all together, reading the strands from file:

tran :: FilePath -> IO String
tran = fmap (roundResult . transTrans . adjoin (read . strand) . (head &&& last))
            . readFASTA

-- Solve this problem for rosalind_tran.txt at this directory

{--
>>> tran "Y2017/M03/D07/rosalind_tran.txt" 
"2.04901960784"
--}
