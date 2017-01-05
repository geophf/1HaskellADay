module Y2017.M01.D05.Exercise where

import Data.Array

-- below imports available from 1HaskellADay git repository

import Data.SAIPE.USStates
import Graph.ScoreCard

import Y2016.M12.D21.Exercise
import Y2016.M12.D22.Exercise
import Y2017.M01.D04.Exercise

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
povertyRatio scorecard = undefined

data Attribs = POPULATION | POVERTY | POVERTYRATIO
             | TOTALDEBT  | PERCAPITADEBT    -- will be used (read further down)
   deriving (Eq, Ord, Enum, Bounded, Ix, Show)

type EnhancedSC = ScoreCard USState Attribs Float

enhancedScoreCard :: ScoreCard USState Axes Float -> EnhancedSC
enhancedScoreCard scorecard = undefined

-- Great. Now. Which US State has the highest poverty ratio? The lowest?

impoverished :: [EnhancedSC] -> [EnhancedSC]
impoverished = undefined

{--
Remember reading in US State total and per capita debt? That was the exercise
for Y2016.M12.D22. Re-read in that information again from:

Y2016/M12/D22/personal_debt_load_by_US_state.csv

And round out the US State scorecard information with those attributes:
--}

augmentWithDebtInfo :: EnhancedSC -> USStateDebt -> EnhancedSC
augmentWithDebtInfo scorecard debtinfo = undefined

{--
Now that you have a set of collated data for US States with debt and poverty
information, is there a correllation? Or: what are the top 5 US States in debt?
Bottom 5? What are the top 5 US State with the highest poverty ratios? Lowest?

Now ratios are very small numbers [0, 1], but populations and debts are very
big ones. Tomorrow we'll level these disparate data sets and look at clustered
results.
--}
--}
