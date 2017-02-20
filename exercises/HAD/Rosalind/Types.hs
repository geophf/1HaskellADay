module Rosalind.Types where

type DNAStrand = String
type Ident = String

data IdxStrand = IS { ident :: Ident, strand :: DNAStrand }
   deriving (Eq, Ord, Show)


