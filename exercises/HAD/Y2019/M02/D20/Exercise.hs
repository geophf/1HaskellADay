module Y2019.M02.D20.Exercise where

{--
Okay, from a set of letters, we have words that can be formed from those 
letters. Great. Now we want to find specific words of lenght n and that has
certain patterns.

Today, we'll look at words that start with _, and later we'll generalize words
of a__ix_q, or whatever patterns.
--}

import Data.Set as Set

import Y2019.M02.D18.Exercise

wordsOfLength :: Dict -> String -> Int -> Set String
wordsOfLength dict ltrs n = undefined

wordsStartingWith :: Dict -> String -> String -> Set String
wordsStartingWith dict ltrs startsWith = undefined

wordsStartingWithOfLength :: Dict -> String -> String -> Int -> Set String
wordsStartingWithOfLength dict ltrs startsWith n = undefined
