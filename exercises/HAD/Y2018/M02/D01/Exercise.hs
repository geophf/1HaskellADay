module Y2018.M02.D01.Exercise where

{--
So there's a clever algorithm to find whether words are anagrams.

So, from the fundamental theorem of arithmetic we know that an integer is an
unique product of primes, so, knowing that, we can substitute letters for 
primesand, boom, our 'hashing algorithm' 'automagically' falls out:

all words with the same prime multiples are anagrams.

So, with that, what are the anagrams in your local dictionary?
--}

import Data.Map (Map)

localDictionary :: FilePath
localDictionary = "/usr/share/dict/words"

-- change to where your local dictionary resides

anagramSets :: [String] -> Map Integer [String]
anagramSets words = undefined

-- given ALL the words anagramSets returns sets of anagrams

-- How many anagram sets are there? Exclude single-word sets: those don't count

-- Which sets have the most words that are anagrams? There may be more than one
