module Puzzles.Mensa.Poesy where

import Prelude hiding (Word)
import Data.List ((\\), intersect)
import Data.Set (Set)
import qualified Data.Set as Set

{--
I have fun with these word-puzzles. I hope you do to. Today's comes from the
Mensa Genius Quiz-a-Day Book by Dr. Abbie F. Salny:

In this type of verse, "first" and "second" and so on refer to the individual
letters of a word. Fine the correct letter for each definition or explanation,
and complete the word.

My first is in sugar, but not in tea.
My second in swim, but not in sea.
My third in apple and also pear.
My fourth in ring and also hare.
My last in ten but not in herd.
My whole: a very complimentary word!
--}

type Word = String

-- to define a word, we need to define a letter of it:

letter :: Rule -> [Char]
letter (NotIn a b) = a \\ b
letter (Also a b) = a `intersect` b

-- so a word is just the mapping of the letters

word :: [Rule] -> [Word]
word = mapM letter

-- now we run the words through a dictionary and get real words, we hope:

nltrdict :: Int -> Rule -> FilePath -> IO (Set Word)
nltrdict n firstLtr = fmap (Set.fromList . filter pare . words) . readFile
   where pare = (&&) . startsWith (Set.fromList (letter firstLtr)) <*> nLtr n

nLtr :: Int -> Word -> Bool
nLtr n = (== n) . length

startsWith :: Set Char -> Word -> Bool
startsWith ltrs = (`Set.member` ltrs) . head

{--
>>> fmap length $ fiveltrdict (head rules) "/usr/share/dict/words"
2171

... that's a lot of words!
--}

data Rule = NotIn Word Word | Also Word Word
   deriving (Eq, Show)

{--
e.g.:

rules :: [Rule]
rules = [NotIn "sugar" "tea", NotIn "swim" "sea", Also "apple" "pear",
         Also "ring" "hare", NotIn "ten" "herd"]

So, with that:
>>> ((`filter` (word rules)) . flip Set.member)
            <$> fiveltrdict (head rules) "/usr/share/dict/words" 
["swart","smart"]

... and "smart" is complimentary. You're looking smart today!
--}
