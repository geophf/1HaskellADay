module Y2018.M02.D14.Exercise where

-- I'm thinking of a word that has the letter 'v' in it, for some strange reason

import Y2018.M02.D01.Exercise

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
import Data.Map (Map)
import Data.Set (Set)

type AnagramSets = Map Integer [String]

wordsOf :: Char -> AnagramSets -> [String]
wordsOf letter sets = undefined

-- But how many v-words are of length ... 5?

len :: Int -> [[a]] -> [[a]]
len n words = undefined

{-- BONUS -----------------------------------------------------------------

So, I'm playing scrabble and the 3rd letter has to be 'a' and the 5th letter
has to be 'r' ...

How many words are like that? How would you go about finding those words?
--}

letterAt :: Char -> Int -> AnagramSets -> Set String
letterAt letter position sets = undefined
