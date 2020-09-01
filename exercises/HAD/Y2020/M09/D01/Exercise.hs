module Y2020.M09.D01.Exercise where

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

import Data.Map (Map)

import Y2020.M08.D26.Exercise (importBook, Text, Library, BookInfo)
import Y2020.M08.D25.Exercise (gutenbergIndex, workingDir, gutenbergTop100Index)
import Y2020.M08.D28.Exercise (cleanDoc, wordFreq)
import Y2020.M08.D31.Exercise (removeStopwords, stopwords, loadStopwords)

type Ontology = Map BookInfo (Map String Int)

bookVec :: Library -> IO Ontology
bookVec books = undefined

{--
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
ontology allbooksvecs = undefined

{--
`ontology` has an interesting type-signature. It takes a map of word-counts
and reduces it to ... an unified word-count.

Hm. Verra in-ter-est-ing. *smoke pipe, pensively.
--}
