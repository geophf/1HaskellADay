module Y2017.M07.D03.Solution where

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

statements :: [AgeRel]
statements = [Tom `IsYoungerThan` Rose,
              Tom `IsOlderThan` Will,
              Tom `IsOlderThan` Jack,

-- 'in that order'? What does that mean? My hazard:

              Will `IsYoungerThan` Jack, -- is this a correct interpretation?

-- carrying on:

              Rose `IsYoungerThan` Susie,
              Rose `IsOlderThan` Jack,
              Jack `IsYoungerThan` Jim,
              Susie `IsOlderThan` Rose, -- obviously
              Susie `IsYoungerThan` Jim,
              Jim   `IsOlderThan` Tom]

-- The answer, at a glance, is Jim, as Tom is younger than Rose, who is younger
-- than Susan, who is younger than Jim who is only older, not younger, than the
-- others. But how do we go about proving it? Well, as Larry Wall says, 
-- TMTOWTDI, or, 'There's More Than One Way To Do It,' so, take your pick. An 
-- easy approach is to use list-sorting logic, I suppose.

oldest :: [AgeRel] -> [Person]
oldest peeps = [b | (a `IsYoungerThan` b) <- peeps,
                    not (any (\(x `IsYoungerThan` y) -> x == b) 
                             (filter ahYouth peeps))]
ahYouth :: AgeRel -> Bool
ahYouth (_ `IsYoungerThan` _) = True
ahYouth _                     = False

{--
>>> oldest statements 
[Jim,Jim]

Which shows there are two paths to arriving at Jim as the eldest.

Do you see them?
--}

{--
Another approach would be to map these statements into an ontology of age-
relations then go to the top (or the bottom) of that graph. That, however, is
an exercise for another day.
--}
