module Y2017.M01.D27.Exercise where

{--
Here's a word puzzle for you, this I found browsing through "The Mensa Genius
Quiz-a-Day Book" and happened upon the problem for October 28th.

Guess the word, letter-by-letter:

My first is in water but not in tears
My second is in listen but not in hears
My third is in three but not in she
My fourth is in clear but not in tea
My last is in hang but not in grand
My whole assembles in a band.

What is the word?
--}

-- first we need a isIn-notIn function, right?

data IsInButNotIn = Chars { isIn :: String, butNotIn :: String }
   deriving (Eq, Show)

isInNotIn :: IsInButNotIn -> [Char]
isInNotIn isinbutnotin = undefined

-- from the above puzzle, derive what the IsInButNotIn values are:

puzzleClues :: [IsInButNotIn]
puzzleClues = undefined

-- then we need to assemble the word from the isInNotIns:

wordsFrom :: [IsInButNotIn] -> [String]
wordsFrom words = undefined

-- But are all of the possibilities words? No. Now we need to filter against
-- a dictionary of acceptable words to find our solution(s)

-- A possible set of acceptable words may be your /usr/share/dict/words file
-- Your computer may have that file elsewhere.

findRealWords :: [String] -> [String] -> [String]
findRealWords dict guesses = undefined

-- What words do you come up with for your solution?
