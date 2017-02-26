module Rosalind.Bases where

-- We represent the bases of DNA A, C, G, T as Haskell values. And stuff.
-- There is a bit at the end of this module that deals with generating random
-- gene sequences then looking at the reverse complement of those sequences.

import Control.Comonad
import Control.Monad.State
import Data.Array
import Data.Char
import Data.Monoid

-- below imports available via 1HaskellADay git repository

import Control.List             -- for List comonad
import Data.Random

data Base = A | T | C | G
   deriving (Eq, Ord, Enum, Bounded, Ix, Show, Read)

allBases :: [Base]
allBases = [A ..]

data NucleotideTriple = Triple (Base, Base, Base)
   deriving (Eq, Ord)

allTriples :: [NucleotideTriple]
allTriples = map Triple [(a,b,c) | a <- allBases, b <- allBases, c <- allBases]

{-- 
For the read instance of a triple, we will specialize on the formatting
we are give which is, e.g.: "tgg"
--}

instance Show NucleotideTriple where
   show (Triple (a, b, c)) = map (head . show) [a, b, c]

mkTrip :: [Base] -> NucleotideTriple
mkTrip [a,b,c] = Triple (a,b,c)

instance Read NucleotideTriple where
   readsPrec prec = readParen False 
     (\r -> [(mkTrip bases, rest) |
                (trechars, rest) <- lex r,
                let bases = map (read . return . toUpper) trechars])

-- so, let triple = Triple (T, G, G) in read (show triple) == triple is True

-- Now, let's talk generating Gene Sequences from 1HAD 2016-02-12:

data GeneSequence = GS { bases :: [Base] }

instance Show GeneSequence where
   show (GS seq) = map (head . show) seq

instance Read GeneSequence where
   readsPrec _ str = [(GS $ map (read . pure) str, "")]

instance Monoid GeneSequence where
   mempty = GS []
   GS a `mappend` GS b = GS (a `mappend` b)

rndGeneSequence :: Monad m => Int -> StateT (RNG Int) m GeneSequence
rndGeneSequence = fmap (GS . map (toEnum . (`mod` 4))) . someRNDs

-- ... return a really long one, like more than 1,000 nucleotides ... we'll
-- be sequencing it for a future exercise, don't you know.

-- (So, yah, I just said, 'return a random gene sequence, but make sure it's
-- long') (deal with the conundrum.)

-- geophf, n.: heart-breaker, dream wrecker, train crasher, conundrum maker.

{--
*Main> rndSeed >>= evalStateT (rndGeneSequence 1000) ~>
AAATGTATGCCCTGAGTGCCGTTCTTGATGCTGTTGATCGCGGATTGCGTGACCCCCTGAGAAATTATTCGGACGTG
AATGTATCGCACCGTTGAGGGCGACACTCCCTGATGTTTTTTGGGAATCATGCAATTAAACATCAGAGCTTGCTGCA
CCGTCAGCAGTGATTTTGTGGTTCTGCCGCACTATGCCAGTTTGAGCCTGTTTGATATGGAGCACTATTGAGTAAAG
GGACTTTCTGTTTGTGAGAACTTCACCTTGGTCGCGCCAACGCGAGCACGTTCGAAATACTAATCACCATCTCTGTG
TGTATTGATGGTACCCCATATACTGTTGATCAGCGCAGACGTACAGCGCGCTTCTACAATAAACTTGTAGGTATGCT
CTCCAAAAAAAGACTAATTTGCCGATTTCCGCTTTTGCGAGACACTTGAGTAAGAATAGGCTCACAGTACGCAACCG
ACGAAAGATGTCTGGTATGCGAAATCCCCTCTGATTGCGGCATTGGGGATCATATAGGAGCATGAGAATCAATTTTT
CCACAGGAACGCGTATGGCATAAACTCTCATCCATCGATACGCCAGCATTACACGATGGACTGAACAATTACCCAGC
CAACTTGGAGTTCTCCCGGACGACAAATCTTAGGTCCGAGCGAGACGCTCAGTTATAAAATCATGTATAACCCCTGA
ACGTCCTTGCGATCGCTTGCCCCGCAAATTATCATTTATGTTCGTAGTAATGGGCGTGGATAGTCGTCTGTATTCTG
CAAGGTCGCTGCCATACAAAGGATTAAGAGTTGTGTTAGATCGCGATTAACGTAGGCTTGGAGACGCACCGAGGAGT
ATTGACCTGGACGGGTATCGCGTCGGAAGACCGCACGCTCGACGAGCTATCCTTTTTGGTGCCGTGATCTGATTAGA
GTACGGTCCAGGCAAATTGGGGGAAACGCAAGGGTTTACAAGACGGGCGATCGAGCGCATAATTATAGCAGTAACC

Okay, now let's separate the above into nucleotide triples.

Here's the thing: in a strand, you don't know where you're 'starting.' So you 
don't know what the triples are, so, say you have this strand:

... GCTCAGATGTTAGAAGGAGATTAGGCGTCAGAAGCTAAGGGTATTCCAGA ...

Where is it's 'beginning'? You don't know. So the leading triples here could be:

[GCT,CAG,ATG,...]  (obviously)

OR, they could be:

[CTC,AGA,TGT, ...] (drop 1)

OR, they could be:

[TCA,GAT,GTT, ...] (drop 2)

So let's do that. How? Comonads, obviously!

First we make the bases of a gene sequence a comonad. We can do this because
we 'know' that a gene sequence is never empty. 'Know' is a technical term:
it means 'know.'
--}

-- Now we chunk-n-dunk? Nah: let's slice-n-dice, instead!

strands :: GeneSequence -> [[NucleotideTriple]]
strands (GS genes) = take 3 (genes =>> slicer)

slicer :: [Base] -> [NucleotideTriple]
slicer (a:b:c:rest) = Triple (a,b,c) : slicer rest
slicer _            = []

{--
So, for the above set of bases:

*Main> let bases = "GCTCAGATGTTAGAAGGAGATTAGGCGTCAGAAGCTAAGGGTATTCCAGA"
*Main> strands (GS $ map (read . return) bases) ~>
[[GCT,CAG,ATG,TTA,GAA,GGA,GAT,TAG,GCG,TCA,GAA,GCT,AAG,GGT,ATT,CCA],
 [CTC,AGA,TGT,TAG,AAG,GAG,ATT,AGG,CGT,CAG,AAG,CTA,AGG,GTA,TTC,CAG],
 [TCA,GAT,GTT,AGA,AGG,AGA,TTA,GGC,GTC,AGA,AGC,TAA,GGG,TAT,TCC,AGA]]

Okay, now let's generate a random set of bases:

*Main> rndSeed >>= evalStateT (rndGeneSequence 1000) ~> 
TTTCACAGAAAGCGTTAGGGGAGGGCCAGGCGTCTTTGAGTGTTCATTTATTC ...

*Main> strands bases ~> 
[[TTT,CAC,AGA,AAG,CGT,TAG,GGG,AGG,GCC,AGG,CGT,CTT,TGA, ...], ...]
--}

-- How about the reverse complement?

reverseComplement :: GeneSequence -> GeneSequence
reverseComplement (GS bases) = GS (map complement (reverse bases))

complement :: Base -> Base
complement A = T
complement T = A
complement G = C
complement C = G

isComplement :: Base -> Base -> Bool
isComplement b = (b ==) . complement
