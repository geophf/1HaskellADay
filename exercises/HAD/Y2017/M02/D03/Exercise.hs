module Y2017.M02.D03.Exercise where 

import Prelude hiding (Word)
import Data.Set (Set)

{--
Let's turn to the Mensa Genuis Quiz-a-Day book again for today's problem.

Then maybe some more Rosalind stuff next week? Or interesting NASA data-science?

We shall see.

So, today's problem is from August 4:

Can you go from PINK to ROSE in four steps, changing one letter at a time and
making a new English word at each step?

P I N K
_ _ _ _
_ _ _ _
_ _ _ _
R O S E

Use a dictionary of English words: mine is at /usr/share/dict/words, yours may
be elsewhere.
--}

type Word = String
type Dictionary = Set Word

englishWords :: FilePath -> IO Dictionary
englishWords = undefined

firstWord, lastWord :: Word
firstWord = "PINK"
lastWord  = "ROSE"

verifyEnglishWord :: Dictionary -> Word -> Bool
verifyEnglishWord = undefined

interimWords :: Dictionary -> Word -> Word -> Int -> [[Word]]
interimWords startWord endWord nWordsToGenerate = undefined

oneLetterDifference :: Word -> Word -> Bool
oneLetterDifference = undefined

-- hints: how do you ensure a progression? That is: how do you make sure you
-- don't rediscover words already used?

-- hint: note the capitalization of the words.
