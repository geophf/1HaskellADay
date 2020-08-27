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
>>> let the_index = gutenbergIndex (workingDir ++ gutenbergTop100Index)
>>> let xmas_info = head . Map.toList <$> the_index
>>> xmas_info
("A Christmas Carol in Prose; Being a Ghost Story of Christmas by Charles Dickens (403)",
 "http://www.gutenberg.org//ebooks/46")
>>> let xmas_carol = importBook =<< xmas_info
>>> length <$> xmas_carol
182057

>>> Map.map getSum . charCount <$> xmas_carol
{('\n',4238),('\r',4238),(' ',28438),('!',411),('"',1388),('#',1),('$',2),
 ('%',1),('\'',456),('(',37),(')',37),('*',28),(',',2906),('-',476),('.',1640),
 ('/',24),('0',20),('1',60),('2',10),('3',13),('4',17),('5',12),('6',11),
 ('7',6),('8',14),('9',9),(':',98),(';',370),('?',159),('@',2),('A',270),
 ('B',199),('C',235),('D',108),('E',187),('F',135),('G',254),('H',261),...}

Now find the 'weird words' in the book's text. What are 'weird words'?
'weird words' are words that you didn't expect would be words but are
words, and they are weirding you out. Words like "\138\016\054" ... and
stuff.
--}

weirdWords :: Text -> Set String
weirdWords book = undefined

{--
>>> let weirds = weirdWords <$> xmas_carol 
>>> weirds
{"\"'And","\"A","\"Ah!\"","\"All","\"Always","\"Am","\"An","\"And",...}

>>> length <$> weirds
3759

So: what are the weird characters in the weird words?
--}

weirdChars :: Set String -> Set Char
weirdChars weirds = undefined

{--
>>> weirdChars <$> weirds
{"!\"#$%'()*,-./0123456789:;?@[]\182\187\191"}
--}

-- with this analysis, we'll be able to write a word-vectorizer, ... tomorrow.
