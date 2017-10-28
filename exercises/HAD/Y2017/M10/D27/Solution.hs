module Y2017.M10.D27.Solution where

{--
Mathematical date puzzle: #haskell #1Liner

today is 2017-10-27

What other valid months/days are anagrams of this year?
--}

import Data.List (permutations)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Time (Day)

mbday :: String -> Maybe Day
mbday = fmap fst . listToMaybe . reads

dayBuilder :: [String]
dayBuilder = map ("2017-" ++) [a:b:'-':c:[d] | [a,b,c,d] <- permutations "2017"]

anagramsOf2017 :: [Day]
anagramsOf2017 = mapMaybe mbday dayBuilder

{--
>>> anagramsOf2017 
[2017-02-17,2017-10-27,2017-01-27,2017-12-07,2017-07-12,2017-07-21]

The whole discussion on readS, etc, on stack overflow is educational:

https://stackoverflow.com/questions/5121371/how-to-catch-a-no-parse-exception-from-the-read-function-in-haskell
--}

{-- BONUS -----------------------------------------------------------------

How many days from today are anagrams of 2017? What are they?
--}

futureDaysOf2017 :: Day -> [Day]
futureDaysOf2017 = flip filter anagramsOf2017 . (<)

{--
>>> futureDaysOf2017 (read "2017-10-27")
[2017-12-07]
--}
