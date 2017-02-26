{-# LANGUAGE ViewPatterns #-}

module Rosalind.AminoAcid where

{--
Continuation of the solution for sequencing amino acids started with 
Rosalind.Bases. Here we map amino acids to their corresponding nucleotide 
triples.
--}

import Control.Arrow
import Data.Array
import Data.Char
import Data.Foldable (toList)
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- below import available via 1HaskellADay git respository

import Rosalind.Bases

data AcidName = A | C | D | E | F | G | H | I | K | L
              | M | N | P | Q | R | S | T | V | W | Y
   deriving (Eq, Ord, Enum, Bounded, Ix, Show, Read)

data AminoAcid = Acid AcidName (Set NucleotideTriple)
   deriving (Eq, Ord)

instance Show AminoAcid where
   show (Acid name triples) =
        show name ++ " : " ++ show (Set.toList triples)

{--
instance Read AminoAcid where
   readsPrec prec = readParen False (\r -> [(acid, rest) |
      (acidName, colonRemainder) <- lex r,
      (":", triplesEtc) <- lex colonRemainder,
      (triples, rest) <- readsPrec prec triplesEtc,
      let acid = Acid (read acidName) (Set.fromList triples)])

-- So!

let triples = read "[GCT, GCC, GCA, GCG]" :: [NucleotideTriple]
let acid = Acid H (Set.fromList triples)

acid ~> H : [GCA,GCT,GCC,GCG]

read (show acid) == acid is True

so now that we're pretty close, we have to write a parser function for the 
given file format.
--}

parseAcidDefs :: FilePath -> IO [AminoAcid]
parseAcidDefs = fmap (map parseAcid . lines) . readFile

-- so the above is one way to do it: import the CODON table from a file.
-- Another way to do it is to define the table internally and use parseAcid.

acidDefs :: [String]
acidDefs = ["a: gct gcc gca gcg","c: tgt tgc","d: gat gac","e: gaa gag",
            "f: ttt ttc","g: ggt ggc gga ggg","h: cat cac","i: att atc ata",
            "k: aaa aag","l: tta ttg ctt ctc cta ctg","m: atg","n: aat aac",
            "p: cct ccc cca ccg","q: caa cag","r: cgt cgc cga cgg aga agg",
            "s: tct tcc tca tcg agt agc","t: act acc aca acg",
            "v: gtt gtc gta gtg","w: tgg","y: tat tac"]

parseAcid :: String -> AminoAcid
parseAcid (words . map toUpper -> ((name:_):rest)) =
   Acid (read $ return name) (Set.fromList $ map read rest)

{--
>>> map parseAcid acidDefs
[[A : [GCA,GCT,GCC,GCG], ...]

start is ATG. I know this
--}

start :: NucleotideTriple
start = read "ATG"

-- stop is merely the dual of the intersection of all possible nucleotide
-- triples to the ones that map to acids (aka 'list difference')

stop :: [(AcidName, NucleotideTriple)] -> [NucleotideTriple]
stop = (allTriples \\) . map snd

{--
>>> stop (sparse acids)
[TAA,TAG,TGA]
--}

stops :: [NucleotideTriple]
stops = stop (sparse (map parseAcid acidDefs))

-- to use stop, we need to simplify the Acid-values to tuples:

sparse :: [AminoAcid] -> [(AcidName, NucleotideTriple)]
sparse = concatMap (sequence . second toList . simplify)

simplify :: AminoAcid -> (AcidName, Set NucleotideTriple)
simplify (Acid n s) = (n, s)

data CODONTable =
   Table (Map NucleotideTriple AcidName) (Map AcidName (Set NucleotideTriple))

data AminoCode = AC AcidName | Stop
   deriving (Eq, Ord, Show)

uncode :: AminoCode -> AcidName
uncode = fromJust . code2Maybe

code2Maybe :: AminoCode -> Maybe AcidName
code2Maybe (AC n) = pure n
code2Maybe Stop = Nothing

-- uncode of Stop is an error

-- now we need to de-sparse the sparseness ... or we can just use the
-- AminoAcid set ...

codonTable :: [AminoAcid] -> CODONTable
codonTable = 
   map simplify                                           >>>
   map swap . concatMap (sequence . second toList) &&& id >>>
   Map.fromList *** Map.fromList                          >>>
   uncurry Table

swap :: (a,b) -> (b,a)
swap = snd &&& fst

codons :: CODONTable
codons = codonTable (map parseAcid acidDefs)

-- Given that we have the codon table, let's extract triples from acids 
-- and vice versa

triples :: CODONTable -> AcidName -> Set NucleotideTriple
triples (Table _ m) = (m Map.!)

{--
>>> triples tab D
{GAT,GAC}
--}

acid :: CODONTable -> NucleotideTriple -> AminoCode
acid (Table m _) = maybe Stop AC . (`Map.lookup` m)

{--
>>> acid tab (read "TAT")
AC Y

>>> acid tab (read "TAA")
Stop

The dual: looking up the triples from the amino acid name.
--}

trips :: CODONTable -> AcidName -> Set NucleotideTriple
trips (Table _ m) = fromMaybe Set.empty . (`Map.lookup` m)

-- And with that, we have the first step to gene sequencing.

-- the below is to solve what triples compose a protein
-- a dynamic-programming exercise

target :: String -> CODONTable -> [[NucleotideTriple]]
target protein (Table _ names) = 
   let aminoAcidNames = map (read . return . toUpper) protein
       aminoAcids = mapMaybe (`Map.lookup` names) aminoAcidNames 
   in  retargeted $ map Set.toList aminoAcids 

{--
But the function targeted is the wrong orientation we want M x N not N x M
so we have to flip/redistribute the triples.
--}

retargeted :: [[NucleotideTriple]] -> [[NucleotideTriple]]
retargeted [] = [[]]
retargeted (trips:rest) = trips >>= (`map` retargeted rest) . (:)
  -- n.b. "trips >>= for (retargeted rest) . (:)" loops. Why?

go :: String -> [[NucleotideTriple]]
go acid = target acid codons

{--
>>> go "kmspdw"
[[AAA,ATG,AGT,CCA,GAT,TGG], ... 95 others]

Cool beans!

Other samples:

>>> go "abdd"
error [b is not an acid]

>>> go "acddtci"
[768 sequences] ... wow!

Notes while solving this exercise:

1 and 2. Given Rosalind.Bases and Rosalind.AminoAcid:

As we already proved that there is at most one amino acid defined by
a nucleotide triple, we improve the type-signature of the acid function
to reflect this semideterminacy

3. 4 * 4 * 4 == 64 

or 

let bases [A, G, T, C] 
in  length [Triple (a, b, c) | a <- bases, b <- bases, c <- bases]

4. solution provided by the go function or target function
--}
