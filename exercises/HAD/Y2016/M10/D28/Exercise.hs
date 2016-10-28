module Y2016.M10.D26.Exercise where

import Data.Array
import Data.Map (Map)
import Data.Time

import Analytics.Trading.Data.Row (Symbol)   -- http://lpaste.net/109658
import Analytics.Trading.Scan.Top5s     -- http://lpaste.net/1443717939034324992

-- the below imports are available from 1HaskellADay git repository

import Graph.KMeans
import Graph.ScoreCard

import Y2016.M10.D19.Exercise (LeadersLosers)
import Y2016.M10.D21.Exercise (top5s2ScoreCards)

{--
So, let's bring this all together. Over the past couple of weeks (imports above)
we have created attributed values for securities in the top5s categories of the
stock markets. These attributed, from the raw data, are leaders and losers in
price and market capitalization and leaders (only) in volume.

The archive of the top5s stocks is at the URL:
https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M10/D17/top5s.csv

We further developed the raw data to include the number of times a security
has shown on any of the top5s lists and then by list (category and loser or
leader of category).

Then we further include runs, that is: number of trading-days-in-a-row, a
stock makes an appearance.

All of these attributed values are integers. Okay.

So, before we've had several data type-kinds to describe these different kinds
of attributed values, let's pull all these various kinds into one indexable
data type.
--}

data StockAttribute = Raw LeadersLosers | Showings | RunFor LeadersLosers
   deriving (Eq, Ord, Show)

-- now we need the above to be indexable, which means the above type-values
-- must also be enumerated. What are the definitions for these type-instances?

instance Enum StockAttribute where
   fromEnum val = undefined
   toEnum n = undefined

instance Ix StockAttribute where
   range (a, b) = undefined
   inRange (a, b) c = undefined
   index (a, b) c = undefined

-- Okay, given the above, take the top5s archive and produce a set of
-- ScoreCards, one per security, with all the above raw and derived attributes

type StockScores a = ScoreCard Symbol StockAttribute a

top5sScoreCards :: FilePath -> IO (Map Symbol (StockScores Int))
top5sScoreCards top5s = undefined

-- now, cluster them. How many clusters makes sense? Are the clusters different
-- from the ones produced in the Y2016.M10.D24.Exercise? How so?

{-- BONUS -----------------------------------------------------------------

Scale all attributes so each attributed value falls in the range 0.0 - 1.0.
Once (re)scaled, cluster the new score cards. What do the new clusters look
like? Same as the unscaled ones? Different? How so?

--}

rescale :: [StockScores Int] -> [StockScores Float]
rescale = undefined

-- hint: rescale may be comonadic.
-- hint: use Graph.KMeans.kmeans to cluster your scorecards.

-- QUESTION ----------------------------------------------------------------

-- kmeans can be slow, there was a paper written that the kmeans algorithm,
-- itself, is inefficient, and there are various solutions proposed to improve
-- the naive k-means algorithm by order(s) of magnitude.

-- So, k-means is slow. Is Graph.KMeans.kmeans unnecessarily slow? Are there
-- inefficiencies in this definition? If so, what improvements do you recommend?
-- Show how you ascertained these inefficiencies. Profiling?
