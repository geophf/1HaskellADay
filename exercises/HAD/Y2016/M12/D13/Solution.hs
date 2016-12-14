module Y2016.M12.D13.Solution where

import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Network.HTTP

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import Control.Scan.CSV

{--
Today we'll be looking at some census data for the U.S.A.

Read in the file at:

http://www.census.gov/did/www/saipe/inputdata/irs.csv

And format to some form that makes sense to you.
--}

type AGI = Int   -- AGI: Adjust Gross Income

-- the IRS (Internal Revenue Service) so does love their TLA
-- (three letter acronyms). Not that I would know that ... personally, but ...

data CensusDatum = CD { pop :: PopulationCount, medianAGI, meanAGI :: AGI }
   deriving Show

type CensusData = Map Year (Map USState CensusDatum)

type URL = FilePath

censusURL :: URL
censusURL = "http://www.census.gov/did/www/saipe/inputdata/irs.csv"

readCensusData :: URL -> IO CensusData
readCensusData url =
{--
   fmap (foldr updateMap Map.empty . map (stateyearline . csv) . drop 5 . lines)

OF COURSE LINES IS BROKEN! I MEAN, WHY WOULD IT BE WORKING!?!?
--}
   fmap (foldr updateMap Map.empty . map (stateyearline . csv) . init . drop 5 . linez)
   (simpleHTTP (getRequest url) >>= getResponseBody)

linez :: String -> [String]
linez = rend '\r'

updateMap :: (USState, Year, CensusDatum) -> CensusData -> CensusData
updateMap (st, yr, datum) m =
   Map.insert yr (Map.insert st datum $ mbyr yr m) m
  
mbyr :: Year -> CensusData -> Map USState CensusDatum
mbyr yr m = case Map.lookup yr m of
   Nothing  -> Map.empty
   Just stm -> stm

stateyearline :: [String] -> (USState, Year, CensusDatum)
stateyearline (_:st:yr:pop:_:_:_:_:_:_:_:med:mean:_) =
   (st, read yr, CD (readStr pop) (readStr med) (readStr mean))
stateyearline x = error ("Could not parse " ++ show x)

readStr :: String -> Int
readStr = read . filter (/= ',')
    -- include (init . taili) to eliminate quotes?
    -- No, csv already ('automagically') eliminates quotes

-- *Y2016.M12.D13.Solution> readStr "\"3,367,441\"" ~> 3367441

-- *Y2016.M12.D13.Solution> readCensusData censusURL ~> census

{--
Now, answer the below questions.

In 2013, which State had the highest mean income?
      ...which State had the lowest?

      ...which State had the largest gap between median and mean income?

In 2013, what is the populate of the USA, based on the 56 (?!?) States listed?
--}

type USState = String
type Year = Int

type Query x = CensusData -> Year -> x

highestMeanIncome, lowestMeanIncome, meanMedianGap :: Query (USState, AGI)
highestMeanIncome = head <<- meanIncomes
lowestMeanIncome  = last <<- meanIncomes

meanMedianGap     = head <<- incomeGaps

{--
*Y2016.M12.D13.Solution> highestMeanIncome census 2013 ~> ("Connecticut",95492)
*Y2016.M12.D13.Solution> lowestMeanIncome census 2013 ~> ("Mississippi",47411)
*Y2016.M12.D13.Solution> meanMedianGap census 2013 ~> ("Connecticut",47168)
--}

meanIncomes, incomeGaps :: Query [(USState, AGI)]
meanIncomes = mapper meanAGI
incomeGaps = mapper ((-) . meanAGI <*> medianAGI)

  -- instead of the more wordy: mapper (uncurry (-) . (meanAGI &&& medianAGI))

mapper :: (CensusDatum -> AGI) -> Query [(USState, AGI)]
mapper f cd = sortBy sorter . Map.toList . Map.map f . (cd Map.!)

sorter :: Ord b => (a,b) -> (a,b) -> Ordering
sorter = compare `on` Down . snd

type PopulationCount = Int

population :: CensusData -> Year -> USState -> PopulationCount
population cd yr state = pop (cd Map.! yr Map.! state)

-- *Y2016.M12.D13.Solution> population census 2013 "Wisconsin" ~> 5075267

-- how would you define the function that sums the population for all USStates?

allStates :: Query [USState]
allStates = Map.keys <<- (Map.!)

usPopulation :: Query PopulationCount
usPopulation cd yr = sum . map (population cd yr) $ allStates cd yr

-- *Y2016.M12.D13.Solution> usPopulation census 2013 ~> 265693697

-- missing some folks but that's what the data is telling us.

{-- BONUS -----------------------------------------------------------------

Why are the population-counts in quotes, and why do they have comma separators
and don't they know it makes it hard for us Haskellers, and why?

... okay, no bonnus question; I'm being fatuous. Maybe.
--}
