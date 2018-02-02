module Y2018.M02.D01.Solution where

{--
So there's a clever algorithm to find whether words are anagrams.

So, from the fundamental theorem of arithmetic we know that an integer is an
unique product of primes, so, knowing that, we can substitute letters for 
primesand, boom, our 'hashing algorithm' 'automagically' falls out:

all words with the same prime multiples are anagrams.

So, with that, what are the anagrams in your local dictionary?
--}

import Control.Arrow ((&&&))
import Data.Array (Array, listArray, (!))
import Data.Char (toUpper, isAlpha)
import Data.Ord
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

-- below imports available via 1HaskellADay git repository

import Control.DList (dlToList, dl')
import qualified Data.MultiMap as MM

localDictionary :: FilePath
localDictionary = "/usr/share/dict/words"

-- change to where your local dictionary resides

{--
>>> hinglish <- words <$> readFile localDictionary
>>> length hinglish 
235886
--}

anagramSets :: [String] -> Map Integer [String]
anagramSets = Map.map dlToList . MM.store . MM.fromList dl' . map (primeWord &&& id)

primeWord :: String -> Integer
primeWord = foldr (*) 1 . map ((primes !) . toUpper) . filter isAlpha

primes :: Array Char Integer
primes = listArray ('A','Z')
   [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101]

-- given ALL the words anagramSets returns sets of anagrams

-- How many anagram sets are there? Exclude single-word sets: those don't count

count :: Map a [b] -> Map a [b]
count = Map.filter doubleton

doubleton :: [a] -> Bool
doubleton (_:_:_) = True
doubleton _       = False

{--
>>> anagrams = count $ anagramSets hinglish
>>> length anagrams
15287
--}

-- Which sets have the most words that are anagrams? There may be more than one

mosties :: Map a [b] -> [(a,[b])]
mosties = sortOn (Down . length . snd) . Map.toList

{--
>>> map (second unwords) (take 5 (mosties anagrams))
[(4191554,"angor argon goran grano groan nagor Orang orang organ rogan Ronga"),
 (2890514,"Elaps lapse Lepas Pales salep saple sepal slape spale speal"),
 (70222834,"asteer Easter easter Eastre reseat saeter seater staree teaser Teresa"),
 (445738,"Antu antu aunt naut taun Tuan tuan Tuna tuna"),
 (476410,"caret carte cater crate creat creta react recta trace")]
>>> map (second length) (take 5 (mosties anagrams))
[(4191554,11),(2890514,10),(70222834,10),(445738,9),(476410,9)]
--}
