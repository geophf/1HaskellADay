module Y2019.M02.D18.Exercise where

{--

Given a set of letters and a word-dictionary (that you locate), come up with
a set of 3-letter words, 4-letter words, ... n-letter words from those letters.

Rule: you can use each letter only once.

So, for example, given:

"meiosp"

The following (and more) words are returned:

[["sop","sip","sim","pie"],["some","mops","pose"],["poise"]]

Do not return 1- or 2-letter words.

I have a word-dictionary on my computer I use, you may find your own
--}

import Data.Char (toLower) -- ... this might help
import Data.Map (Map)
import Data.Set (Set)

wordDictionary :: FilePath
wordDictionary = "/usr/share/dict/words"

-- Problem 1: return 3+-letter words

type WordsByLength = Map Int (Set String)
type Dict = WordsByLength

wordsByLength :: FilePath -> IO Dict
wordsByLength dict = undefined

-- returns { (3, set of three-letter words), ...}

wordsFrom :: Dict -> String -> WordsByLength
wordsFrom dict letters = undefined

-- e.g.: wordsFrom dict "meiosp"
-- ->> {(3,["sop","sip","sim","pie"]),(4,["some","mops","pose"]), ...}

-- this function may help:

wordContainsOnly :: String -> String -> Bool
wordContainsOnly letters word = undefined

{-- 
Returns true if word uses some of the characters of letters only once, so:

>>> wordContainsOnly "meiosp" "hello"
False
>>> wordContainsOnly "meiosp" "me"
True
>>> wordContainsOnly "meiosp" "mess"
False
--}
