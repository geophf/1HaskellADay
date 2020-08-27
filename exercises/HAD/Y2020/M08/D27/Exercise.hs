module Y2020.M08.D27.Exercise where

{--
OKAY! now we can start vectorizing these documents into word-vectors.

But ... wait.

What, exactly, is a word? I don't have a good answer to that quesstion, because
why? because some characters are word-parts, and some aren't, and given UTF-8
and documents that use words from multiple languages, answering this question
may require some analysis on its own.

Today's #haskell problem.

Take a document, and analyze its characters. What are the characters? What
are the character-counts?

That's one layer of analysis.

Now, do the same thing, but break up the document into words.

What are the characters of words? Are there any surprises? Does the function
`words` need to be rewritten to accommodate words with special characters?

(Or, do we need to eliminate or modify special characters to separate words
in inadvertently compound words? Or, on the other hand, join word-parts
inadvertently separated by special characters?)
--}

import Data.Map (Map)
import Data.Set (Set)

import Y2020.M08.D26.Exercise (importBook, Text)

type CharFreq = Map Char Int

charCount :: Text -> CharFreq
charCount book = undefined

{-- 
Now find the 'weird words' in the book's text. What are 'weird words'?
'weird words' are words that you didn't expect would be words but are
words, and they are weirding you out. Words like "\138\016\054" ... and
stuff.
--}

weirdWords :: Text -> Set String
weirdWords book = undefined

-- with this analysis, we'll be able to write a word-vectorizer, ... tomorrow.
