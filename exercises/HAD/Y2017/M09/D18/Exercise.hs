module Y2017.M09.D18.Exercise where

import Data.Map (Map)
import Data.Ratio ((%))

-- below import available via 1HaskellADay git repository

{--
Today we will take a dictionary of 'things' and determine their relative
'strength.'

Say you have a mapping of words and their occurrences in a document (e.g.: the
solution to Y2017.M09.D15.Exercise). With that mapping compute a new mapping:
--}

type Strength a = Map a Rational

wordStrength :: Map String Int -> Strength String
wordStrength wordcounts = undefined

-- wordStrength computes the total number of words in a document from the
-- occurrence count and outputs the 'strength' of each word where:

strength :: Integer -> Integer -> Rational
strength totalWords wordCount = wordCount % totalWords

-- compute the strengths of the words in the documents in
-- Y2017/M09/D08/articles/n/
