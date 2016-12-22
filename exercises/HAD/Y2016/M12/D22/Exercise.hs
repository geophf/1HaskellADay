module Y2016.M12.D22.Exercise where

import Network.HTTP.Conduit

-- below imports available from 1HaskellADay git repository

import Control.Scan.CSV
import Data.Monetary.USD
import Data.SAIPE.USStates

{--
Now, for something completely different

... data-set-wise

Located in this directory is the personal debt load per US State:

Y2016/M12/D22/personal_debt_load_by_US_state.csv

or at the URL:

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M12/D22/personal_debt_load_by_US_state.csv

We're going to look at integrating the US poverty data with the US personal
debt load data and see if there's some kind of correspondence, but not today.

Today's Haskell exercise let's load these data into Haskell, when you've done
that, answer the below questions.
--}

data USStateDebt = SomeStructureThatDeclaresDebtsUSState

type URL = FilePath

readUSStateDebtData :: URL -> IO [USStateDebt]
readUSStateDebtData url = undefined

-- Now that you have these data, answer the below questions

-- 1. Which 5 US States have the most debt in total? The least 5?
-- 2. Which 5 US States have the most debt per person? The least 5?
-- 3. Which 5 US States fall into the top5s in both 1. and 2.?

totalDebtPerStateRanked, personalDebtPerStateRanked :: [USStateDebt] -> [USStateDebt]
totalDebtPerStateRanked = undefined
personalDebtPerStateRanked = undefined
