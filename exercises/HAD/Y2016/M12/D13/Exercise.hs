module Y2016.M12.D13.Exercise where

import Network.HTTP

{--
Today we'll be looking at some census data for the U.S.A.

Read in the file at:

http://www.census.gov/did/www/saipe/inputdata/irs.csv

And format to some form that makes sense to you.
--}

data CensusData = SomeSensibleFormYouDeclare

type URL = FilePath

readCensusData :: URL -> IO CensusData
readCensusData url = undefined

{--
Now, answer the below questions.

In 2013, which State had the highest mean income?
      ...which State had the lowest?

      ...which State had the largest gap between median and mean income?

In 2013, what is the populate of the USA, based on the 56 (?!?) States listed?
--}

type USState = String
type Year = Int

type StateQ = CensusData -> Year -> USState

highestMeanIncome, lowestMeanIncome, meanMedianGap :: StateQ
highestMeanIncome = undefined
lowestMeanIncome  = undefined
meanMedianGap     = undefined

type PopulationCount = Int

population :: CensusData -> Year -> USState -> PopulationCount
population = undefined

-- how would you define the function that sums the population for all USStates?

{-- BONUS -----------------------------------------------------------------

Why are the population-counts in quotes, and why do they have comma separators
and don't they know it makes it hard for us Haskellers, and why?

... okay, no bonnus question; I'm being fatuous. Maybe.
--}
