module Y2020.M11.D27.Exercise where

-- 'Yesterday,' or 'one turkey-ago' (now an unit of measure): ...

import Y2020.M11.D25.Solution     -- for Morse-type, and stuff

{--
... we laid out the type for Morse code, and I also said there was a graph
that represented morse code, but then I blithely (I love that word: 'blithely')
implemented morse code by hand from a messed up wikitable.

THE. NERVE.

But: doing things by hand is 'faster,' right?

Cha. Shur.

But what ... 'asshurances' do we have that the by-hand product is viable?

Were there any duplicates in 'yesterday's/one turkey-ago's'-table?
--}

import Data.Char (toUpper)
import Data.Map (Map)
import Data.Set (Set)

import Data.Relation

import Graph.Query

duplicates :: MorseTable -> Map [Morse] [Char]
duplicates = undefined

{-- 
Okay, so there were (weren't) duplicates. Fine. But, were all the 
representations correct? Tougher question. And that's today's Haskell problem:
to build the morse table the 'right' way: automated, from the graph.

But that means we first have to build the graph.
--}

data Letter = START_HERE | Chr Char
   deriving (Eq, Ord, Show)

instance Node Letter where
   asNode = undefined

instance Edge Morse where
   asEdge = undefined

-- we use morse-code.png to build our graph

type MorseRel = Relation Letter Morse Letter

morseGraph :: [MorseRel]
morseGraph = 
   [Rel START_HERE DIT (Chr 'E'), Rel START_HERE DA (Chr 'T')]
   ++ translateStringToMorseRels
           ("e.i i.s s.h s-v i-u u.f e-a a.r r.l a-w w.p w-j t-m m-o "
         ++ "t.n n-k k-y k.c n.d d-x d.b m.g g.z g-q")

translateStringToMorseRels :: String -> [MorseRel]
translateStringToMorseRels = undefined

-- okay, but how many (unique) 'letters' are there in this graph?

uniqueLetters :: [MorseRel] -> Set Letter
uniqueLetters = undefined

-- now, using the above graph, spit out the morse table ... THE SEQUEL:

morseTable :: [MorseRel] -> MorseTable
morseTable = undefined

-- in this NEW AND IMPROVED Morse table, are there any duplicate?

anyDuplicatesInTheNewAndImprovedMorseTable :: MorseTable -> Map [Morse] [Char]
anyDuplicatesInTheNewAndImprovedMorseTable = undefined

-- are there any differences between the old and unimproved table and the new one?

morseTableDifferences :: MorseTable -> MorseTable -> Set Char
morseTableDifferences = undefined

-- and, using our NEW AND IMPROVED MORSE TABLE!!!, translate the following:

konamiCode :: String
konamiCode = "Up up down down left right left right b a"

----- BONUS -------------------------------------------------------

-- Upload these relations to the graph store:

-- yeah, using cyphIt, or something like that.
