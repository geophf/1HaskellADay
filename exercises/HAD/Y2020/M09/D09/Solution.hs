module Y2020.M09.D09.Solution where

{--
Yesterday (which, oddly enough, is really yesterday, in this particular case),
--}

import Y2020.M09.D08.Solution
import Y2020.M09.D01.Solution (Ontology, ontology)

{--
we did a modicum of data analyses and reduced a word-... set? word-... net?
from ~250,000 words to ~2,000 words.

We can work with either set, but 2,000 words, approximating standard daily
usage is an avenue I'd like to explore.

But here's a problem, ... an administrative problem.

To get to the 2,000 words, we need to load in all the documents, load in all
the words, good and bad, then reduce to those 2,000 words.

For today's #haskell problem, let's compile those 2,000 words into a Haskell
program, so we have those words right away, and get rid of that pesky monad: IO.
--}

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

type ModuleName = String

compile :: Ontology -> ModuleName -> IO ()
compile bookwordnet modularus =
   let filename = modularus ++ ".hs"
       lebooks = pprint "bookwordnet" bookwordnet
       wordscounts = ontology bookwordnet
       lewc = pprint "wordscounts" wordscounts
       header = heading modularus
   in  writeFile filename (unlines (header ++ lebooks ++ lewc))

heading :: String -> [String]
heading m = ["module " ++ m ++ " where", "", "import Data.Map", ""]

kv :: (Show a, Show b) => Int -> (a, b) -> String
kv indent entry = replicate indent ' ' ++ show entry

pprint :: (Show a, Show b) => String -> Map a b -> [String]
pprint var mappo = [var ++ " = fromList ["]
                 ++ [intercalate ",\n" (map (kv 6) (Map.toList mappo))]
                 ++ ["   ]"]

{--
>>> putStrLn (unlines (pprint "foo" (Map.fromList [("bar", 1),("quux",2)])))
foo = Map.fromList [
      ("bar",1),
      ("quux",2)
   ]

NOW THAT'S MORE LIKE IT! :<

The function `compile` compiles `bookwordnet` to a Haskell module, named in
`modularus` and placed that Ontology value and the corresponding 
WordOccurrences value into that file, so, when imported, is available for
immediate use.

DO IT TO IT!
--}

loadNCleanOntology :: IO Ontology
loadNCleanOntology = ont >>= \mont ->
   let wc = ontology mont
       iab = inAllBooks wc
       ooow = onlyOneOccurringWords wc
       ooib = oneWordInBook mont
       newOnt = removeInfreqs (Set.unions [iab, ooow, ooib]) mont
   in  return newOnt

{--
>>> loadNCleanOntology >>= flip compile "GutenbergTop100"

$ head GutenbergTop100.hs
module GutenbergTop100 where

import Data.Map (Map)
import qualified Data.Map as Map

bookwordnet = Map.fromList [...]

$ ghci GutenbergTop100.hs
...
GutenbergTop100.hs:102:126: error:
    • Variable not in scope: fromList :: [([Char], Integer)] -> a
    • Perhaps you meant ‘Map.fromList’ (imported from Data.Map)
      Perhaps you want to add ‘fromList’ to the import list
      in the import of ‘Data.Map’ (GutenbergTop100.hs:3:1-21).
...

oops! one more improvement, then! (changed qualified Data.Map import to
unqualified. Anddddd:

% ghci GutenbergTop100.hs

>>> length wordscounts 
2333
>>> length bookwordnet 
100

NOW! we can get down to business!
--}

{-- BONUS! -------------------------------------------------------

Eh. Why NOT save the large ontology? We can analyze and mine it later if
we'd like, or we can let it languish, gathering bit-rot, sad, ... and alone.

OR NOT, BECAUSE, AT SOME FUTURE DATE, WE SHALL USE THAT KNOWLEDGE AND RULE
THE WORLD! MWA-HAHA!

... either way.

>>> ont >>= flip compile "BigGutes"

$ ls -l GutenbergTop100.hs 
-rw-r--r--  1 geophf  staff  378072 Sep 11 11:58 GutenbergTop100.hs
$ ls -l BigGutes.hs       
-rw-r--r--  1 geophf  staff  19396513 Sep 11 12:16 BigGutes.hs

$ ghci BigGutes.hs 

... um, this is taking a long time to load ... :/
--}
