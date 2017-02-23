module Y2017.M02.D23.Exercise where

import Control.Arrow (second)
import Network.HTTP.Conduit

-- below imports from 1HaskellADay git repository 'may' help reading FASTA files

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
baseURL = "http://www.uniprot.org/uniprot/uniprot_id/"

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

-- as expected output

motifNglycos :: FilePath -> IO (Maybe (AccessID, [Location]))
motifNglycos url = undefined

-- verify that the sample access IDs produce the set of results

-- Now, sew the definitions together to produce the expected output

motifs :: String -> IO String
motifs urls = undefined

{--
Note

Some entries in UniProt have one primary (citable) accession number and some 
secondary numbers, appearing due to merging or demerging entries. In this 
problem, you may be given any type of ID. If you type the secondary ID into the 
UniProt query, then you will be automatically redirected to the page containing 
the primary ID. You can find more information about UniProt IDs at this URL:

http://www.uniprot.org/help/accession_numbers
--}
