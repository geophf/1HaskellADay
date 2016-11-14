module Y2016.M11.D14.Exercise where

-- available from 1HaskellADay git repository

import Data.Relation

{--
Today's exercise comes from the Mensa Genuis Quiz-a-Day Book, the succ November 14
puzzle:

As Amy was strolling one day with her baby daughter in her carriage, she met
her husband's mother's only daughter-in-law's sister's husband. What relationship
was this man to Amy?

Simply enough, right?

Now this is a puzzle of (familial) relationships, so: show this result as
a relationship set.
--}

data Role = Husband | Mother | DaughterInLaw | Sister
   deriving (Eq, Show)
data Person = Amy | Person Role
   deriving (Eq, Show)

-- question, is there some trickiness with Eq? OF COURSE NOT?
-- So is there some logical equivalence we should use instead? Hm.

data Relationship = MARRIED_TO | PARENT | SIBLING
   deriving (Eq, Show)

type FamilyTree = [Relation Person Relationship Person]
amyMet :: FamilyTree
amyMet = undefined

{-- BONUS ----------------------------------------------------------------

Using the graphing tool of your choice, draw the relationships. Show your
results. Awwww! A happy family!
--}

amyGraph :: FamilyTree -> IO ()
amyGraph amysFam = undefined
