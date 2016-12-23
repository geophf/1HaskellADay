module Y2016.M12.D22.Solution where

import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sortBy, intersect)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Network.HTTP.Conduit (simpleHttp)

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
--}

debtURL :: URL
debtURL = "https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M12/D22/personal_debt_load_by_US_state.csv"

{--
We're going to look at integrating the US poverty data with the US personal
debt load data and see if there's some kind of correspondence, but not today.

Today's Haskell exercise let's load these data into Haskell, when you've done
that, answer the below questions.
--}

data USStateDebt = USStDebt { name :: USState, stateDebt, perCapitaDebt :: USD }
   deriving (Eq, Show)

type URL = FilePath

readUSStateDebtData :: URL -> IO [USStateDebt]
readUSStateDebtData = simpleHttp >=>
   return . mapMaybe (line2Debt . csv) . tail . lines . BL.unpack

line2Debt :: [String] -> Maybe USStateDebt
line2Debt (st:tot:cap:_) = Map.lookup st convmap >>= \state ->
   return (USStDebt state (readUSD tot) (readUSD cap))

readUSD :: String -> USD
readUSD = read . filter (/= ',')

{--
*Y2016.M12.D22.Solution> let rows = readUSStateDebtData debtURL ~>
[USStDebt {name = Alabama, stateDebt = $68343597000.00, perCapitaDebt = $14173.00},
 USStDebt {name = Alaska, stateDebt = $29780396000.00, perCapitaDebt = $40714.00},...]

-- Now that you have these data, answer the below questions
--}

totalDebtPerStateRanked, personalDebtPerStateRanked :: [USStateDebt] -> [USStateDebt]
totalDebtPerStateRanked = sortBy (compare `on` stateDebt)
personalDebtPerStateRanked = sortBy (compare `on` perCapitaDebt)

{--
-- 1. Which 5 US States have the most debt in total? The least 5?

least 5:
*Y2016.M12.D22.Solution> fmap (take 5 . map name . totalDebtPerStateRanked) rows ~> bottot
[South Dakota,Vermont,North Dakota,Wyoming,Nebraska]

most debt 5:
*Y2016.M12.D22.Solution> fmap (take 5 . map name . reverse . totalDebtPerStateRanked) rows ~> toptot
[California,New York,Texas,Illinois,Ohio]

-- 2. Which 5 US States have the most debt per person? The least 5?

least personal debt 5:
*Y2016.M12.D22.Solution> fmap (take 5 . map name . personalDebtPerStateRanked) rows ~> botpers
[Tennessee,Nebraska,Indiana,Wisconsin,South Dakota]

most personal debt 5:
*Y2016.M12.D22.Solution> fmap (take 5 . map name . reverse . personalDebtPerStateRanked) rows ~> toppers
[Alaska,Hawaii,Connecticut,Ohio,Illinois]

-- 3. Which 5 US States fall into the top5s in both 1. and 2.?

bottom-ers:
*Y2016.M12.D22.Solution> bottot `intersect` botpers 
[South Dakota,Nebraska]

top-ers:
*Y2016.M12.D22.Solution Data.List Control.Applicative> toptot `intersect` toppers 
[Illinois,Ohio]

A lesson learned here for me is this:

liftA2 intersect toppers toptot 

if toppers and toptot are in the IO monad keeps going back to the IO-source,
the URL, to read the original data each time, apparently because we cannot 
assume that the data at the URL is static. So I kept hitting the server until
I got a secure server exception from the host. ick. So, if you're in the IO
monad, stay in that domain until you're done with that computation.
--}
