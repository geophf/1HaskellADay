module Y2017.M07.D03.Exercise where

{--
It's a question of Ord.

From the Mensa Genius Quiz-a-Day Book by Dr. Abbie F. Salny, July 1 problem:

Tom is younger than Rose, but older than Will and Jack, in that order. Rose is
younger than Susie, but older than Jack. Jack is younger than Jim. Susie is
older than Rose, but younger than Jim. Jim is older than Tom. Who is the oldest?
--}

data Person = Tom | Rose | Will | Jack | Susie | Jim
   deriving (Eq, Show)

data AgeRel = IsOlderThan Person Person | IsYoungerThan Person Person
   deriving (Eq, Show)

oldest :: [AgeRel] -> Person
oldest peeps = undefined

-- Question: is there a better, more accurate, type for oldest?
