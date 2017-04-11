module Y2017.M04.D12.Exercise where

import Prelude hiding (Word)

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

word :: [Rule] -> [Word]
word rules = undefined

data Rule = NotIn Word Word | Also Word Word
   deriving (Eq, Show)

rules :: [Rule]
rules = undefined    -- what are the rules that help to complete the word?
