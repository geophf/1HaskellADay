module Y2020.M08.D21.Exercise where

import Y2020.M08.D19.Exercise

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
import Data.Set (Set)

type Length = Int

data Hint = SomeDataStructureYouComeUpWith

type Word = String
type WordList = Map Int (Set Word)

chooseFrom :: WordList -> Length -> [Hint] -> Set Word
chooseFrom wordlist wordLength hints = undefined

{--
And we've already loaded our word lists from the solution to 
Y2020.M08.D19.Exercise

What do you get for chooseFrom 5 [] <$> wordList?
What do you get from chooseFrom 5 [Hint (3,'e')] <$> wordList?
What do you get from chooseFrom 5 [Hint (3,'e'), Hint (1,'l')] <$> wordList?

>>> let wordList = wordLengths <$> allWords dicts

>>> length <$> wordList
24
--}
