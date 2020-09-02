module Y2020.M09.D01.Solution where

{--
OKAY! SCROOGE!

So, we ingested and word-counted "A Christmas Carol," removing the weirdness
and removing the stopwords.

GREAT!

... oh, I forgot: I'm Scrooge.

BAH! HUMBUG!

So, now, let's ingest and word-count the top-100 books from project gutenberg.

That's going to be the easy part of today's Haskell exercise.
--}

import Control.Monad ((>=>))

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord

import Data.Set (Set)
import qualified Data.Set as Set

import Y2020.M08.D26.Solution (importLibrary, Library, BookInfo)
import Y2020.M08.D25.Solution (gutenbergIndex, workingDir, gutenbergTop100Index)
import Y2020.M08.D28.Solution (cleanDoc, wordFreq)
import Y2020.M08.D31.Solution (removeStopwords, stopwords, loadStopwords)

import qualified Data.Bag as Bag

type Ontology = Map BookInfo (Map String Int)

bookVec :: Set String -> Library -> Ontology
bookVec stops =
     let weirdos = Set.fromList "!\"#$%'()*,-./0123456789:;?@[]\182\187\191"
     in  Map.map (removeStopwords stops . wordFreq . cleanDoc weirdos)

{--
>>> let nupes = loadStopwords stopwords
>>> let idx = gutenbergIndex (workingDir ++ gutenbergTop100Index)
>>> let lib = idx >>= importLibrary
>>> let ont = bookVec <$> nupes <*> lib
>>> take 2 . Map.keys <$> ont
[("A Christmas Carol in Prose; Being a Ghost Story of Christmas by Charles Dickens (403)",
  "http://www.gutenberg.org//ebooks/46"),
 ("A Dictionary of Cebuano Visayan by John U. Wolff (256)",
  "http://www.gutenberg.org//ebooks/40074")]

Okay, the more-fun part:

Which words are common among books? From our word-counts, create a frequency
analysis of words mentioned across books. So, Scrooge may have only one 
reference (unless, of course, he is mentioned in several books. I don't know:
maybe Jane Austen liked reading "A Christmas Carol"!*

*Yes, I am fully aware of the anachronism), and maybe "upon" is mentioned in
several of the gutenberg top 100, so its cross-book-count may possible be higher
than "scrooge," for example.

You tell me.
--}

ontology :: Ontology -> Map String Int
ontology = Bag.asMap . Bag.fromList . concat . map Map.keys . Map.elems

{--
`ontology` has an interesting type-signature. It takes a map of word-counts
and reduces it to ... an unified word-count.

Hm. Verra in-ter-est-ing. *smoke pipe, pensively.

>>> take 10 . sortOn (Down . snd) . Map.toList . ontology <$> ont
[("body",100),("ebook",100),("ebooks",100),("free",100),("gutenberg",100),
 ("library",100),("page",100),("project",100),("search",100),("start",100)]

... hmmm, it seems gutenberg's introductory text is skewing the results :/

Let's see what the results look like, removing the 100% hit-counts:

>>> let verds = Map.filter (not . (== 100)) . ontology <$> ont
>>> length <$> verds
251419

... but, I examined verds, and some of those verds ... sorry: words, are a 
straight-up mess, but oh, well.

>>> take 10 . sortOn (Down . snd) . Map.toList $ verds
[("asked",99),("does",99),("domain",99),("follow",99),("help",99),("think",99),
 ("something",97),("broken",96),("whether",96),("wrong",93)]

THAT's more like it!
--}
