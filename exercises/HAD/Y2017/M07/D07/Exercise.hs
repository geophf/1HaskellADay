module Y2017.M07.D07.Exercise where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

{--
Today is the seventh day of the seventh month of the seven(teen)th year.

WHAT DOES IT ALL MEAN?

Well, for me, it means ... pause ... wait, for effect ...

SCRABBLE DAY!

So, given the scrabble tiles on your scrabble rack, make a word of length
2, 3, 4, 5, 6, or 7 letters and maximize your score

TODAY's word is Triple Word Score.

So, give a word, and compute the score for it.

WHO GETS THE HIGHEST SCORE TODAY?!?
--}

rack :: String
rack = "AAYWRTM"

scores :: Map Char Int
scores = Map.fromList (zip "ABCDEGYWPRSTMU" [1,3,3,2,1,2,4,4,3,1,1,1,3,1])

legalWords :: FilePath -> Set String
legalWords url = undefined

-- from a dictionary url or file (e.g. /usr/share/dict/words) get a set of legal
-- words.

essaying :: String -> Map Char Int -> Set String -> [(String, Int)]
essaying rack scores dict = undefined

-- from the above, propose a set of words and their scores. What is the highest
-- scoring word that you can find? What is the maximum possible score for the
-- rack (remember the triple word score bonus!)

{-- BONUS -----------------------------------------------------------------

Also, these racks
--}

bonusRacks :: [String]
bonusRacks = words "AEYRRCR AURTSBP AAEUDGR"

-- For THESE racks, the first rack has a 3rd letter double bonus, the other
-- racks do not have a bonus at all.
