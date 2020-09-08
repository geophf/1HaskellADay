module Y2020.M09.D08.Exercise where

{--
Yesterday* ...
--}

import Y2020.M09.D01.Exercise

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

import Data.Map as Map
import Data.Set as Set

type WordOccurrences = Map String Int
 
-- 0. How many words are in all the books?

allWordsCount :: WordOccurrences -> Int
allWordsCount wordcounts = undefined

-- WordOccurrences come from Y2020.M09.D01.Exercise.ontology

-- 1. How many words, and what are the words that occur in all 100 books?

inAllBooks :: WordOccurrences -> Set String
inAllBooks wordcounts = undefined

-- 2. How many words, and what are the words, that occur only once?

onlyOneOccurringWords :: WordOccurrences -> Set String
onlyOneOccurringWords wordcounts = undefined

-- 3. Which words occur only once in a book (any book) ... even if those words
-- occur in multiple books, if they occur just once in any book, what are these
-- words?

oneWordInBook :: Ontology -> Set String
oneWordInBook bookswords = undefined

-- 4. Okay. Remove all the one-word and all-books-words from out ontology.
-- Recompute the WordOccurrences from that trimmed Ontology. What is 
-- allWordsCount now?

-- BONUS -------------------------------------------------------

-- 5. Repeat the above for words in 99 books and only 2 books. What are the
-- results from that trimming?

