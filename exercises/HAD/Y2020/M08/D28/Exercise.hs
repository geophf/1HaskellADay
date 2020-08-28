module Y2020.M08.D28.Exercise where

{--
Clean words. Today let's make those words clean. What words, you ask? Welp,
one answer is: "alles o' dems wordz!" so, ... yeah: that.

Yesterday, we found we have some weird words in a document we were scanning,
and, also, we found the weird characters in those weird words.

So.

Let's remove the weird characters FROM the document, THEN rescan the document.
Voila, and I hope, we will have a scanned document of clean words.

If only it were that simple (play weird, moody, and forboding music here).
--}

import Control.Monad ((>=>))

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord

import Data.Set (Set)

import Y2020.M08.D25.Solution (gutenbergIndex, workingDir, gutenbergTop100Index)
import Y2020.M08.D26.Exercise (importBook, Text)

-- a helpful function to import a document to analyze:

study :: FilePath -> IO Text
study = gutenbergIndex >=> importBook . head . Map.toList

encleanifyDoc :: Set Char -> Text -> Text
encleanifyDoc weirdChars doc = undefined

{--
`encleanify` takes a set of weird characters and removes those weird characters
from a document. N.B.: not all weird characters are weird characters. What?

From yesterday, we saw that the weird characters for a specific doc were:

{"!\"#$%'()*,-./0123456789:;?@[]\182\187\191"}

But we know that '-' is a word-connector, ... for example: "word-connector"
is a word that has '-' as one of its characters.

[... ya see wha' I dun did wid dat? lol ...]

Are there any other characters that you know and love and want to deweirdify?

Also, the word "don't" but not the words "'A" and "Proposal,'" in the phrase:
"'A Modest Proposal,'" ... so ... simple? easy? straightforward?

Hmmmm.

>>> let bookus = study (workingDir ++ gutenbergTop100Index) 

>>> length <$> bookus 
182057

>>> let weirdos = Set.fromList "!\"#$%'()*,-./0123456789:;?@[]\182\187\191"
>>> let subweirds = deweirdify weirdos (Set.fromList "-'")
>>> let cleenBook = encleanifyDoc subweirds <$> bookus

>>> length <$> cleenBook
174776
--}

deweirdify :: Set Char -> Set Char -> Set Char
deweirdify weirdos nonweirdos = undefined

{--
After you magically encleanify the document, let's encleanify EACH WORD!

*EEK*!

>>> let weirdos = Set.fromList "!\"#$%'()*,-./0123456789:;?@[]\182\187\191"
>>> deweirdify weirdos (Set.fromList "-'")
fromList "!\"#$%()*,./0123456789:;?@[]\182\187\191"
--}

encleanifyWord :: Set Char -> String -> String

-- I ... think (?) if we remove weird characters at the bookends of the word, 
-- it's encleanified? Let's give that a try

encleanifyWord weirdos word = undefined

{--
>>> encleanifyWord weirdos  "'asffag;;;."
"asffag"

>>> encleanifyWord weirdos "don't"
"don't"

After you've encleanified the document and the words of the document, you
can now create a word-count (there's that pesky '-', again, and "there's" has
that pesky inlined '\'' in it, too! ... IT'S TURTLES ALL THE WAY DOWN, I TELL
YA!).

We'll look at the problem of 'stop-words' (often called "STOPWORDS" (caps
intentional)), ... but not today.
--}

cleanDoc :: Set Char -> Text -> [String]
cleanDoc weirds book = undefined

{--
>>> let bookwords = cleanDoc weirdos <$> bookus
>>> length <$> bookwords 
31530
--}

wordFreq :: [String] -> Map String Int
wordFreq encleanifiedDoc = undefined

-- I mean: you can wordFreq a weirded-out doc, but why?
-- Also ... it ... 'MIGHT'(?) be nice to have all the words be of uniform case?
-- ... but then ... proper nouns? Or is that an issue for Named Entity
-- recognizers to deal with?

{--
>>> let wordus = wordFreq <$> bookwords
>>> take 5 . sortOn (Down . snd) . Map.toList <$> wordus
[("the",1742),("and",1118),("of",778),("a",761),("to",738)]
--}

{-- BONUS -------------------------------------------------------

Thought-experiment:

So, I'm thinking about weirdos being (Char -> Bool) as opposed to Set Char.
There are trade-offs to either approach. What are they? Redeclare the above
functions to use weirdo as one type or the other. What impact does that have
on your approaches and implementations to the problem solution?
--}
