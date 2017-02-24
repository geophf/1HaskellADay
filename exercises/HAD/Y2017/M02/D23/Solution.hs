{-# LANGUAGE OverloadedStrings #-}

module Y2017.M02.D23.Solution where

import Control.Arrow (second, app)
import Control.Comonad
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Network.HTTP

-- below imports from 1HaskellADay git repository 'may' help reading FASTA files

import Control.List -- for Comonadic lists
import Control.Scan.CSV (rend)

import Rosalind.Types
import Rosalind.Scan.FASTA

{--
Finding a Protein Motif solved by 2726 as of February 22nd, 2017

Rosalind problem: http://rosalind.info/problems/mprt/

Motif Implies Functionclick to collapse

As mentioned in “Translating RNA into Protein”, proteins perform every practical
function in the cell. A structural and functional unit of the protein is a 
domain: in terms of the protein's primary structure, the domain is an interval 
of amino acids that can evolve and function independently.

Each domain usually corresponds to a single function of the protein (e.g., 
binding the protein to DNA, creating or breaking specific chemical bonds, etc.).
Some proteins, such as myoglobin and the Cytochrome complex, have only one 
domain, but many proteins are multifunctional and therefore possess several 
domains. It is even possible to artificially fuse different domains into a 
protein molecule with definite properties, creating a chimeric protein.

Just like species, proteins can evolve, forming homologous groups called protein
families. Proteins from one family usually have the same set of domains, 
performing similar functions; see Figure 1.

Figure 1. The human cyclophilin family, as represented by the structures of the 
isomerase domains of some of its members.

A component of a domain essential for its function is called a motif, a term 
that in general has the same meaning as it does in nucleic acids, although many 
other terms are also used (blocks, signatures, fingerprints, etc.) Usually 
protein motifs are evolutionarily conservative, meaning that they appear without
much change in different species.

Proteins are identified in different labs around the world and gathered into 
freely accessible databases. A central repository for protein data is UniProt, 
which provides detailed protein annotation, including function description, 
domain structure, and post-translational modifications. UniProt also supports 
protein similarity search, taxonomy analysis, and literature citations.

Problem

To allow for the presence of its varying forms, a protein motif is represented 
by a shorthand as follows: [XY] means "either X or Y" and {X} means "any amino 
acid except X." For example, the N-glycosylation motif is written as N{P}[ST]{P}.

You can see the complete description and features of a particular protein by its
access ID "uniprot_id" in the UniProt database, by inserting the ID number into
--}

baseURL :: String
baseURL = "http://www.uniprot.org/uniprot/"

{--
Alternatively, you can obtain a protein sequence in FASTA format by following

http://www.uniprot.org/uniprot/uniprot_id.fasta

For example, the data for protein B5ZC00 can be found at 

http://www.uniprot.org/uniprot/B5ZC00.

Given: At most 15 UniProt Protein Database access IDs.

Return: For each protein possessing the N-glycosylation motif, output its given 
access ID followed by a list of locations in the protein string where the motif 
can be found.
--}

type AccessID = String
type Location = Int

sample :: AccessID
sample = unlines ["A2Z669","B5ZC00","P07204_TRBM_HUMAN","P20840_SAG1_YEAST"]

result :: [(AccessID, [Location])]
result = map (second (map read . words))
            [("B5ZC00","85 118 142 306 395"),
             ("P07204_TRBM_HUMAN","47 115 116 382 409"),
             ("P20840_SAG1_YEAST","79 109 135 248 306 348 364 402 485 501 614")]

-- or:

output :: String
output = unlines ["B5ZC00","85 118 142 306 395","P07204_TRBM_HUMAN",
                  "47 115 116 382 409","P20840_SAG1_YEAST",
                  "79 109 135 248 306 348 364 402 485 501 614"]

{-- 
To read in a FASTA file from an URL we do:
>>> fmap (readStrands . lines) 
         (simpleHTTP (getRequest (baseURL ++ "A2Z669.fasta")) >>= getResponseBody)

Now we just need to look for the protein-motif by writing a protein-motif-compiler.

But maybe we'll do that another day, as we have the rules of engagement already.
--}

type Proneme = Char -> Bool

-- the nGlycos proneme motif is:

nglycos :: [Proneme]
nglycos = [(== 'N'),(/= 'P'),(||) . (== 'S') <*> (== 'T'),(/= 'P')]

isMotif :: [Proneme] -> String -> Bool
isMotif motif = and . zipWith (curry app) motif

{--
>>> zipWith (curry app) nglycos "MKNKFKTQEELVNHLKT"
[False,True,False,True]
--}

-- as expected output

readFASTAfromURL :: AccessID -> IO IdxStrand
readFASTAfromURL id = fmap (IS id . strand . head . readStrands . lines)
   (simpleHTTP (getRequest (constructURL id)) >>= getResponseBody)

constructURL :: AccessID -> FilePath
constructURL id = baseURL ++ (head (rend '_' id)) ++ ".fasta"

-- okay, from a set of pronemes and a protein string, let's parse out locations

scanner :: [Proneme] -> DNAStrand -> [Location]
scanner motif strand = enumerateTrues 1 (strand =>> isMotif motif)

enumerateTrues :: Int -> [Bool] -> [Location]
enumerateTrues _ [] = []
enumerateTrues x (pred:rest) = (if pred then (x:) else id)
                               (enumerateTrues (succ x) rest)

motifNglycos :: AccessID -> IO (Maybe (AccessID, [Location]))
motifNglycos id = fmap (orNada id . scanner nglycos . strand) $ readFASTAfromURL id

orNada :: AccessID -> [Location] -> Maybe (AccessID, [Location])
orNada _ [] = Nothing
orNada id loc@(_:_) = Just (id, loc)

{-- verify that the sample access IDs produce the set of results
>>> motifNglycos "B5ZC00"
Just ("B5ZC00",[85,118,142,306,395])

SWEET!
--}

-- Now, sew the definitions together to produce the expected output

motifs :: String -> IO String
motifs ids = mapM motifNglycos (lines ids) >>= \ans ->
   let ens = concatMap enstringify (catMaybes ans) in
   mapM_ putStrLn ens >> return (unlines ens)

enstringify :: (AccessID, [Location]) -> [String]
enstringify (id, locs) = [id, intercalate " " (map show locs)]

{--
>>> fmap (== output) $ motifs sample 
B5ZC00
85 118 142 306 395
P07204_TRBM_HUMAN
47 115 116 382 409
P20840_SAG1_YEAST
79 109 135 248 306 348 364 402 485 501 614
True

Note

Some entries in UniProt have one primary (citable) accession number and some 
secondary numbers, appearing due to merging or demerging entries. In this 
problem, you may be given any type of ID. If you type the secondary ID into the 
UniProt query, then you will be automatically redirected to the page containing 
the primary ID. You can find more information about UniProt IDs at this URL:

http://www.uniprot.org/help/accession_numbers
--}

{-- BONUS -----------------------------------------------------------------

For the file rosalind_mprt-3.txt at this directory or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2017/M02/D23/rosalind_mprt-3.txt

read in that file and find the Nglycos motifs of the proteins there enumerated

>>> readFile "rosalind_mprt-3.txt" >>= motifs
P01217_GLHA_BOVIN
80 106
Q05865
389
P42098_ZP3_PIG
124 146 179 271
Q16775
41
P06765_PLF4_RAT
82
P08514_ITAB_HUMAN
46 280 601 711 962
P22457_FA7_BOVIN
185 243
P00744_PRTZ_BOVIN
59 191 289
P10761_ZP3_MOUSE
146 273 304 327 330
P12763_A2HS_BOVIN
99 156 176
P47002
35 552 608
Q8LCP6
259 464 484
A4J5V5
24 38 230
P07306_LECH_HUMAN
79 147
--}
