module Y2017.M01.D05.Solution where

import Control.Arrow ((&&&))
import Data.Array
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

-- below imports available from 1HaskellADay git repository

import Data.Monetary.Currency (value)
import Data.SAIPE.USStates
import Graph.ScoreCard

import Y2016.M12.D21.Solution
import Y2016.M12.D22.Solution
import Y2017.M01.D04.Solution

{--
So, yesterday we looked at US State SAIPE data. The solution showed us the
two standout states: Wyoming had the smallest population and the least poverty,
and California had the largest population and the most poverty.

But 'least' and 'most' can be measured several ways. Yesterday, we simply looked
at the number of people in poverty, but did not take into account the total
population of the State. Who knows? Maybe California has the least poverty if
one adjusts for population? Yes? No?

Let's find out.

Today's Haskell exercise.

Read in the SAIPE data from

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

collating by US State (see yesterday's exercise), then, enhance the score-card
with the ratio of people in poverty to the entire population:
--}

type PovertyRatio = Float -- really a poverty / population ration, but okay

povertyRatio :: ScoreCard a Axes Float -> PovertyRatio
povertyRatio = ((/) . (! Poverty) <*> (! Population)) . values

data Attribs = POPULATION | POVERTY | POVERTYRATIO
             | TOTALDEBT  | PERCAPITADEBT    -- will be used (read further down)
   deriving (Eq, Ord, Enum, Bounded, Ix, Show)

type EnhancedSC = ScoreCard USState Attribs Float

enhancedScoreCard :: ScoreCard USState Axes Float -> EnhancedSC
enhancedScoreCard sc@(SC state arr) =
   SC state (listArray (POPULATION, POVERTYRATIO)
                      ([arr ! Population, arr ! Poverty, povertyRatio sc]))

-- Great. Now. Which US State has the highest poverty ratio? The lowest?

impoverished :: [EnhancedSC] -> [EnhancedSC]
impoverished = sortBy (compare `on` (! POVERTYRATIO) . values)

{--
*Y2017.M01.D05.Solution> readSAIPEUSStateData "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" ~> states
*Y2017.M01.D05.Solution> let enhsts = map enhancedScoreCard states
*Y2017.M01.D05.Solution> head enhsts 
SC {idx = Alabama, values = [(POPULATION,4736374.0),(POVERTY,875853.0),
                             (POVERTYRATIO,0.18492058)]}

*Y2017.M01.D05.Solution> let improv = impoverished enhsts 

least impoverished:

*Y2017.M01.D05.Solution> head improv
SC {idx = New Hampshire, values = [(POPULATION,1288048.0),(POVERTY,108293.0),
                                   (POVERTYRATIO,8.407528e-2)]}

most impoverished:

*Y2017.M01.D05.Solution> last improv 
SC {idx = Mississippi, values = [(POPULATION,2896612.0),(POVERTY,638919.0),
                                 (POVERTYRATIO,0.22057459)]}

Remember reading in US State total and per capita debt? That was the exercise
for Y2016.M12.D22. Re-read in that information again from:

Y2016/M12/D22/personal_debt_load_by_US_state.csv

And round out the US State scorecard information with those attributes:
--}

debtInfoAugmenter :: EnhancedSC -> USStateDebt -> EnhancedSC
debtInfoAugmenter (SC state arr) debtinfo = 
   let conv f = fromRational . value . f in
   SC state (array (POPULATION, PERCAPITADEBT)
             (assocs arr ++ [(TOTALDEBT, conv stateDebt debtinfo),
                             (PERCAPITADEBT, conv perCapitaDebt debtinfo)]))

{--
So, here's the thing: the [EnhancedSC] may not be in the same order as the
[USStateDebt] ... so we have to match by US State... MAP TO THE RESCUE!
--}

augmentWithDebtInfo :: [EnhancedSC] -> [USStateDebt] -> [EnhancedSC]
augmentWithDebtInfo states =
  let mapped = Map.fromList (map (idx &&& id) states) in
  mapMaybe (\debt -> 
      Map.lookup (name debt) mapped >>= return . flip debtInfoAugmenter debt)

{--
*Y2017.M01.D05.Solution> readUSStateDebtData debtURL ~> debts
*Y2017.M01.D05.Solution> let auggies = augmentWithDebtInfo enhsts debts
*Y2017.M01.D05.Solution> head auggies 
SC {idx = Alabama, values = [(POPULATION,4736374.0),(POVERTY,875853.0),
                             (POVERTYRATIO,0.18492058),(TOTALDEBT,6.8343595e10),
                             (PERCAPITADEBT,14173.005)]}

Now that you have a set of collated data for US States with debt and poverty
information, is there a correllation? Or: what are the top 5 US States in debt?
Bottom 5? What are the top 5 US State with the highest poverty ratios? Lowest?

debts:
*Y2017.M01.D05.Solution> let sortedDebts = sortBy (compare `on` (! TOTALDEBT) . values) auggies 

bottom 5:
*Y2017.M01.D05.Solution> mapM_ (print . (idx &&& (! TOTALDEBT) . values)) (take 5 sortedDebts)
(South Dakota,7.707458e9)
(Vermont,7.866666e9)
(North Dakota,9.263742e9)
(Wyoming,9.951523e9)
(Nebraska,1.3139045e10)

top5:
*Y2017.M01.D05.Solution> mapM_ (print . (idx &&& (! TOTALDEBT) . values)) (take 5 (reverse sortedDebts))
(California,7.779184e11)
(New York,3.8746567e11)
(Texas,3.4094422e11)
(Illinois,3.213541e11)
(Ohio,3.2134077e11)

California takes the cake!

Poverty ratios:
*Y2017.M01.D05.Solution> let sortedPovertyRatios = sortBy (compare `on` (! POVERTYRATIO) . values) auggies 

Lowest poverty ratios:
*Y2017.M01.D05.Solution> mapM_ (print . (idx &&& (! POVERTYRATIO) . values)) (take 5 sortedPovertyRatios)
(New Hampshire,8.407528e-2)
(Maryland,9.949522e-2)
(Minnesota,0.1018338)
(Alaska,0.103974395)
(Vermont,0.104290456)

highest poverty ratios:
*Y2017.M01.D05.Solution> mapM_ (print . (idx &&& (! POVERTYRATIO) . values)) (take 5 (reverse sortedPovertyRatios))
(Mississippi,0.22057459)
(New Mexico,0.19827485)
(Louisiana,0.19504689)
(Arkansas,0.18727748)
(Alabama,0.18492058)

Now ratios are very small numbers [0, 1], but populations and debts are very
big ones. Tomorrow we'll level these disparate data sets and look at clustered
results.

Added charts and CSV files of resulting data analyses:

Y2017/M01/D05/01-poverty-us-state.png
Y2017/M01/D05/02-total-debts-us-state.png
Y2017/M01/D05/debt-by-us-state.csv
Y2017/M01/D05/poverty-by-us-state.csv
--}
