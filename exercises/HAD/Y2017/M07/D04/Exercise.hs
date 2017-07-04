module Y2017.M07.D04.Exercise where

{--
Another Mensa-puzzler from the Genius Mensa Quiz-a-Day Book by Dr. Abbie F. Salny.

July 2 problem:

Jemima has the same number of brothers as she has sisters, but her brother,
Roland, has twice as many sisters as he has brothers. How many boys and girls
are there in this family?
--}

data Name = Jemima | Roland
   deriving (Eq, Show)

data Sex = Male | Female
   deriving (Eq, Show)

data Person = Pers Name Sex
   deriving (Eq, Show)

type Boys = Int
type Girls = Int
type Multiplier = Int  -- to equate girls to boys

data Statement = Has Person Multiplier
   deriving (Eq, Show)

jemima, roland :: Statement
jemima = Pers Jemima Female `Has` 1
roland = Pers Roland Male `Has` 2

familySize :: [Statement] -> [(Boys, Girls)]
familySize statements = undefined
