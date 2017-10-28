module Y2017.M10.D27.Exercise where

{--
Mathematical date puzzle: #haskell #1Liner

today is 2017-10-27

What other valid months/days are anagrams of this year?
--}

import Data.Maybe
import Data.Time (Day)

mbday :: String -> Maybe Day
mbday dateStr = undefined

dayBuilder :: [String]
dayBuilder = undefined -- builds the potential 'dates' to test for validity

anagramsOf2017 :: [Day]
anagramsOf2017 = undefined

{-- BONUS -----------------------------------------------------------------

How many days from today are anagrams of 2017? What are they?
--}

futureDaysOf2017 :: Day -> [Day]
futureDaysOf2017 today = undefined
