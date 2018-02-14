{-# LANGUAGE ViewPatterns #-}

module Y2018.M02.D14.Solution where

-- I'm thinking of a word that has the letter 'v' in it, for some strange reason

import Y2018.M02.D01.Solution

{--
So from the above import, we have sets of words associated with a hash. This
hash is the product of their letters-as-primes. AND we know which letter is 
which prime:

>>> primes ! 'A'
2

Voila!

So, given our anagramSets, how many 'English' words have a 'v' in them?
--}

import Data.Array
import Data.Char (toUpper, isLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((-|))

type AnagramSets = Map Integer [String]

wordsOf :: Char -> AnagramSets -> [String]
wordsOf ((primes !) . toUpper -> p) = 
   concat . Map.elems . Map.filterWithKey (\k v -> k `mod` p == 0)

{--
>>> hinglish <- words <$> readFile localDictionary 
>>> anas = anagramSets hinglish 
>>> length anas
215843
>>> vwords = wordsOf 'V' anas
>>> length vwords
19412
>>> take 10 vwords 
["V","v","ava","cava","Ave","ave","Eva","vag","iva","Vai"]
--}

-- But how many v-words are of length ... 5?

len :: Int -> [[a]] -> [[a]]
len n = filter ((== n) . length)

{--
>>> fivers = len 5 vwords 
>>> length fivers
526
>>> take 5 fivers
["abave","cavae","bhava","ajava","Cavia"]
--}

{-- BONUS -----------------------------------------------------------------

So, I'm playing scrabble and the 3rd letter has to be 'a' and the 5th letter
has to be 'r' ...

How many words are like that? How would you go about finding those words?
--}

letterAt :: Char -> Int -> AnagramSets -> Set String
letterAt c (pred -> pos) =
   Set.fromList
 . mapMaybe (\w -> mbhead (drop pos w) >>= \l -> toUpper l == toUpper c -| Just w)
 . wordsOf c

mbhead :: [a] -> Maybe a
mbhead (h:_) = Just h
mbhead []    = Nothing

{--
>>> a3 = letterAt 'A' 3 anas
>>> r5 = letterAt 'R' 5 anas
>>> a3r5 = Set.intersection a3 r5
>>> length a3r5 
674
>>> take 5 (Set.toList a3r5)
["Amalrician","Anacreon","Anacreontic","Anacreontically","Anacrogynae"]

... uh, yeah, how about words on the scrabble board ... so: nine letters
max and no proper nouns:

>>> scrabble = Set.filter (\wrd -> length wrd < 10 && isLower (head wrd)) a3r5
>>> length scrabble
258
>>> take 6 (Set.toList scrabble) 
["alacrify","alacrity","amacratic","amacrinal","amacrine","amatrice"]

I defined this solution with ALACRITY! YUS!
--}
