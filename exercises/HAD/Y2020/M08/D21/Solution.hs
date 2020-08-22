module Y2020.M08.D21.Solution where

import Y2020.M08.D19.Solution (dicts)
import Y2020.M08.D20.Solution (allWords)

{--
Gimme a hint? ... or two?

So, we have the words (all 1-word words, so: NO NEW ZEALAND FOR MEEEEH!) *cries*

*ahem*

So, we have the words, we have the word lengths (from the day before).

Now.

What happens when we get a hint, or two, or some?

Say you get a hint, for example: "The fourth letter is an 'e'."

How do you model hints? How do you incorporate hints into choosing your words?

Today's Haskell problem: model hints and use them to narrow your word-search.
--}

import Prelude hiding (Word)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Letter = Char
type Index = Int
type Length = Int
data Hint = Hint (Index, Letter)

type Word = String
type WordList = Map Int (Set Word)

wordLengths :: Set Word -> WordList
wordLengths = foldl (alterer f) Map.empty
   where f Nothing = Set.singleton
         f (Just set) = flip Set.insert set
         alterer f map val = 
            let len = length val
                base = Map.lookup len map
            in  Map.insert len (f base val) map

{--
>>> Map.map length . wordLengths <$> allWords dicts 
{(1,26),(2,139),(3,1294),(4,4994),(5,9972),(6,17462),(7,23713),(8,29842),...}

... it'd be nice if the printer curtailed long evaluations, ... like most
prolog systems do. #justsayin
--}

chooseFrom :: Length -> [Hint] -> WordList -> Set Word
chooseFrom len hints = maybe Set.empty (cf' hints) . Map.lookup len

cf' :: [Hint] -> Set Word -> Set Word
cf' hints = Set.filter (all' (map hintFilter hints))

hintFilter :: Hint -> Word -> Bool
hintFilter (Hint (pos, letter)) = (== letter) . (!! pos)

all' :: [a -> Bool] -> a -> Bool -- so: a 'dual' of `all`
all' [] _ = True
all' (h:t) wrd = h wrd && all' t wrd

{--
And we've already loaded our word lists from the solution to 
Y2020.M08.D19.Exercise

What do you get for chooseFrom 5 [] <$> wordList?
What do you get from chooseFrom 5 [Hint (3,'e')] <$> wordList?
What do you get from chooseFrom 5 [Hint (3,'e'), Hint (1,'l')] <$> wordList?

where wordList = wordLengths <$> allWords dicts

>>> length <$> wordList
24

>>> chooseFrom 5 [] <$> wordList
{"aalii","aaron","abaca","aback","abaff","abaft","abama","abase",...}
>>> length it
9934

>>> chooseFrom 5 [Hint (3,'e')] <$> wordList
{"abbey","abies","abler","abner","abnet","abret","achen","acher",...}
>>> length it
1335

>>> chooseFrom 5 [Hint (3,'e'), Hint(1,'l')] <$> wordList
{"albee","alces","alder","alfet","alien","alkes","allen",...}
>>> length it
53
--}
