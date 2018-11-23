module Y2018.M11.D23.Exercise where

{--
A puzzle game from

https://www.logic-puzzles.org/game.php?u2=63d3ded6b70a71eac63ba911fadacc6e

The Springfield County Bird Club met today, and several of its members brought
their newest acquisitions. Match each girl to her newest bird - determine its 
species and the month in which it was purchased.


1. Ida's pet was bought 1 month before the parrot.
2. Tamara's pet, the lorikeet and the bird bought in February are all different birds.
3. The finch was bought 1 month before Ida's pet.
4. The bird bought in February is either the finch or Ellen's pet.
5. Ellen's pet is either the canary or the bird bought in February.

Here are the data values under consideration:
--}

data Month = January | February | March | April
   deriving (Eq, Ord, Show)

data Bird = Canary | Finch | Lorikeet | Parrot
   deriving (Eq, Show)

data Girl = Alberta | Ellen | Ida | Tamara
   deriving (Eq, Show)

data Answer = Ans Girl Bird Month
   deriving (Eq, Show)

-- how do you represent a Clue as a Haskell term?

data Clue = IDK

-- and given the above clues, solve the puzzle

solver :: [Clue] -> [Answer]
solver clues = undefined
