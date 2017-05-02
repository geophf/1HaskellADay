module Y2017.M05.D02.Solution where

import Prelude hiding (Word)
import Data.Set (Set)
import qualified Data.Set as Set

-- below import available via 1HaskellADay git repository

import Puzzles.Mensa.Poesy

{--
Another in/not-in puzzler-poem from Mensa Genius Quiz-a-Day Book by
Dr. Abbie F. Salny

To start the month of May off right, figure out the word concealed in the 
following poem. ('First,' 'second,' etc refer to the letters of a word.) By
selecting the right letters you will come up with an appropriate word.

My first is in silly but not in fool.
My second is in pupil but not in school.
My third is in read and also write.
My fourth is in glimpse and also light.
My fifth is in ten but not in three.
My last is in glad and also glee.
My whole is a season of the year
That's clue enough to solve it here!
--}

-- as you see from Puzzles.Mensa.Poesy imported above, you've solved this
-- kind of puzzle before. Define word and rules so that you solve today's
-- problem

rules :: [Rule]
rules = [NotIn "silly" "fool", NotIn "pupil" "school", Also "read" "write",
         Also "glimpse" "light", NotIn "ten" "three", Also "glad" "glee"]

{--
>>> ((`filter` (word rules)) . flip Set.member)
        <$> nltrdict 6 (head rules) "/usr/share/dict/words" 
["spring","spring"]

I have a hard time choosing between "spring" and "spring" ... which one should
I go with? AHA!
--}
