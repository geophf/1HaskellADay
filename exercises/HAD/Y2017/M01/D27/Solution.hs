module Y2017.M01.D27.Solution where

import Control.Monad (guard)
import Data.List ((\\), nub)
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available from 1HaskellADay git repository

import Control.List (takeout)
import Control.Logic.Frege (assert)

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
isInNotIn (Chars isin butnotin) = nub isin \\ butnotin

{--
*Y2017.M01.D27.Solution> map isInNotIn puzzleClues 
["w","litn","tr","clr","h"]
--}

-- from the above puzzle, derive what the IsInButNotIn values are:

puzzleClues :: [IsInButNotIn]
puzzleClues =
    map (uncurry Chars)
        (zip (words "water listen three clear hang")
             (words "tears hears she tea grand"))

{--
*Y2017.M01.D27.Solution> puzzleClues 
[Chars {isIn = "water", butNotIn = "tears"},
 Chars {isIn = "listen", butNotIn = "hears"},
 Chars {isIn = "three", butNotIn = "she"},
 Chars {isIn = "clear", butNotIn = "tea"},
 Chars {isIn = "hang", butNotIn = "grand"}]

butbutbut, geophf! Didn't you just construct a Prolog fact-table?

me: hm? What on Earth is your meaning? <<- totally innocent
--}

-- then we need to assemble the word from the isInNotIns:

wordsFrom :: [IsInButNotIn] -> [String]
wordsFrom = recombine . map isInNotIn

recombine :: [String] -> [String]
recombine [] = [[]]
recombine (h:t) = takeout h >>= \(l,_) -> recombine t >>= return . (l:)

{--
*Y2017.M01.D27.Solution> wordsFrom puzzleClues 
["wltch","wltlh","wltrh","wlrch","wlrlh","wlrrh","witch","witlh","witrh",
 "wirch","wirlh","wirrh","wttch","wttlh","wttrh","wtrch","wtrlh","wtrrh",
 "wntch","wntlh","wntrh","wnrch","wnrlh","wnrrh"]
--}

-- But are all of the possibilities words? No. Now we need to filter against
-- a dictionary of acceptable words to find our solution(s)

-- A possible set of acceptable words may be your /usr/share/dict/words file
-- Your computer may have that file elsewhere.

findRealWords :: Set String -> [String] -> [String]
findRealWords dict guesses = guesses >>= assert (`Set.member` dict)

-- What words do you come up with for your solution?

{--
We get the 'w' five-letter words from /usr/share/dict/words:

*Y2017.M01.D27.Solution>
    fmap (filter ((&&) . ((== 5) . length) <*> ((== 'w') . head)) . words)
         (readFile "/usr/share/dict/words") ~> wfivers
*Y2017.M01.D27.Solution> length wfivers ~> 274

*Y2017.M01.D27.Solution> findRealWords (Set.fromList wfivers) (wordsFrom puzzleClues)
["witch"]

"My whole assembles in a [coven]" more accurate, but also more telling, be-like.
--}
