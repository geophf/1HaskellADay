module Y2020.M09.D08.Solution where

{--
Yesterday* ...
--}

import Y2020.M09.D01.Solution

{--
*time is so fluid for me, ... it being time, and all ... *rolleyes*

... we build this city ...

... wait: we built this ONTOLOGY of words to novels in the top 100-read
books in gutenberg.

OR. DID. WE?

I concluded that exercise with: "I noticed there are gutenberg-artefacts, so
I'm removing words that occur in all 100 books."

But was that a valid assertion?

What other assertions can we make of these data we collected?

Today's Haskell Exercise: data analyses.
--}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Y2020.M08.D25.Solution (gutenbergIndex, workingDir, gutenbergTop100Index)
import Y2020.M08.D26.Solution (importLibrary)
import Y2020.M08.D31.Solution (stopwords, loadStopwords)

type WordOccurrences = Map String Int
 
-- A little helper-function to load the ontology

ont :: IO Ontology
ont = let nupes = loadStopwords stopwords
          idx = gutenbergIndex (workingDir ++ gutenbergTop100Index)
          lib = idx >>= importLibrary
      in  bookVec <$> nupes <*> lib

{--
>>> ont 
... zillions of entries later ...
>>> let mont = it

... `mont` stands for `my ont(ology)`

... now we need to get the WordOccurrences

>>> let wc = ontology mont
--}

-- 0. How many words are in all the books?

allWordsCount :: WordOccurrences -> Int
allWordsCount = length

{--
>>> allWordsCount wc
251429 

That took a little while.
--}

-- 1. How many words, and what are the words that occur in all 100 books?

inAllBooks :: WordOccurrences -> Set String
inAllBooks = Map.keysSet . Map.filter (== 100)

{--
>>> let iab = inAllBooks wc
>>> iab
{"body","ebook","ebooks","free","gutenberg","library",
 "page","project","search","start"}

>>> length iab
10
--}

-- 2. How many words, and what are the words, that occur only once?

onlyOneOccurringWords :: WordOccurrences -> Set String
onlyOneOccurringWords = Map.keysSet . Map.filter (== 1)

{--
Hm:

>>> let ooow = onlyOneOccurringWords wc
>>> ooow

lots of messy words, but then we also have words of this form:

...  "liveries\226\128\157" ...

It looks like we may need to clean up our clean-up algorithm, because:

>>> length ooow
195035

Is a lot of words. Some of those words may be useful in clustering (later).
--}

-- 3. Which words occur only once in a book (any book) ... even if those words
-- occur in multiple books, if they occur just once in any book, what are these
-- words?

oneWordInBook :: Ontology -> Set String
oneWordInBook = Set.unions . map onlyOneOccurringWords . Map.elems

{--
>>> let ooib = oneWordInBook mont
>>> ooib

... lots of words, again ...

>>> length ooib
205354

Good Heavens!
--}

-- 4. Okay. Remove all the one-word and all-books-words from out ontology.

removeInfreqs :: Set String -> Ontology -> Ontology
removeInfreqs = Map.map . flip (foldr ri')

-- well, to remove words from the ontology, we have to remove a word from
-- each book word-count.

ri' :: String -> Map String Int -> Map String Int
ri' = Map.delete

-- ummm ... that was easy. TOO EASY! :<

{--
>>> let newOnt = removeInfreqs (Set.unions [iab, ooow, ooib]) mont
>>> length newOnt
100

... as expected, as there are still 100 books being codified, but ...

>>> let newWc = ontology newOnt 
>>> allWordsCount newWc
2333

Wow! Huge difference! Good or bad?

We will find that out on another, fine Haskell-problem-solving day!
--}
