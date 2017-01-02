module Y2017.M01.D02.Exercise where

import Data.List
import Data.Set (Set)

{--
Happy New Year, Haskellers.

Dr. Paul Coxon claims 2017 will be less divisive than 2016.

He is correct, but let's prove it.

1. What are the unique divisors of last year and this year?
--}

type Year = Int

uniqueFactors :: Year -> Set Int
uniqueFactors yr = undefined

{--
So:

uniqueFactors 2017 will get you {1,2017}
uniqueFactors 2016 will get you {1,4,7,8,9}

2. What are the unique prime factors of each of these years?
--}

primeFactors :: Year -> Set Int
primeFactors = undefined

{--
So:

primeFactors 2017 ~> {2017}
primeFactors 2016 ~> {2,3,7}

Question: 

1. What years AD 2 - AD 2017 are prime?
--}

primeYears :: [Year] -> Set Year
primeYears yrs = undefined

-- 2. Order those years (AD 2 - AD 2017) by the number of their factors
-- 2a. Which year(s) had the most unique factors
-- 2b (or not 2b) (I love writing that) Which years had the most prime factors?

orderedYears :: (Year -> Set Int) -> [Year] -> [(Year, Set Int)]
orderedYears factoringFn years = undefined

-- Hint: you may wish to use Data.List.sortBy and Data.List.group to answer 2.

