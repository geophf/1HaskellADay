{-# LANGUAGE ViewPatterns #-}

module Y2016.M12.D21.Solution where

import Control.Monad (join)
import Data.Array
import qualified Data.Map as Map

-- below imports available via @1HaskellADay git repository

import Control.DList

import Data.SAIPE.USCounties

import Graph.ScoreCard
import Graph.KMeans

import Y2016.M12.D15.Solution
import Y2016.M12.D20.Solution

{--
So, now that we have the US county data, and as ScoreCard values.

Cluster these data.
--}

data Axes = Population | Poverty
   deriving (Eq, Ord, Show, Enum, Bounded, Ix)

type SAIPEScoreCards = [ScoreCard USCounty Axes Float]

saipeRows2SC :: [[String]] -> SAIPEScoreCards
saipeRows2SC = concatMap (\row -> join $ righto (`mkSC` row) (stateContext (row !! 3)))

righto :: (b -> c) -> Either a b -> [c]
righto f = either (const []) (pure . f)

mkSC :: (County, StateAbbrev) -> [String] -> SAIPEScoreCards
mkSC (meld -> count) = righto (SC (read count) . arrayify . snd) . line2SAIPERow

arrayify :: SAIPERow -> Array Axes Float
arrayify row =
   listArray (Population, Poverty) (map fromIntegral [pop row, poverty row])

clusterSAIPE :: SAIPEScoreCards -> IO (SCClusters USCounty Axes Float)
clusterSAIPE counties = let (gens, clusters) = kmeans 30 counties in
   putStrLn ("Clustered in " ++ show gens ++ " generations.") >>
   return clusters

{--

... Right ("Kalawao County",SAIPE {pop = *** Exception: Prelude.read: no parse

well, shoot!

We have to account for population of NA in the original readStr function.

... done.

*Y2016.M12.D21.Solution> readSAIPERaw "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" ~> rows
*Y2016.M12.D21.Solution> let ans = rows >>= clusterSAIPE . saipeRows2SC 
*Y2016.M12.D21.Solution> fmap length ans
Clustered in 148 generations.
4

And do this study:

1) what is the 'optimal'/most-makes-sense number of clusters? 4
2) what is the most populous cluster, county-population-wise and number of
   ScoreCard values-wise? What is the least populous cluster, same criteria?

Well we have this:
{(23,(Sum {getSum = 17},a DList)),
 (24,(Sum {getSum = 150},a DList)),
 (25,(Sum {getSum = 1},a DList)),
 (27,(Sum {getSum = 2974},a DList))}

so cluster # 27 has the most scorecards (2974) and cluster #25 has the least (1)

To examime populations, we have to dig into the DLists.

*Y2016.M12.D21.Solution> Map.map (floor . sum . map ((! Population) . values) . toList . snd) scanned ~>
fromList [(23,46344824),(24,112238240),(25,10011595),(27,144881824)]

so cluster #27 is most populous and 25 is least populous

('least populous' meaning one county has 10 million people!):

*Y2016.M12.D21.Solution> toList . snd $ scanned Map.! 25 ~> 
[SC {idx = Los Angeles County (CA), values = array (Population,Poverty)
                       [(Population,1.0011595e7),(Poverty,1675802.0)]}]

... of course it's Los Angeles County.

Further study:
3) what associations do you see? Are there obvious City vs. Country differences?
   Other differences that are obvious?
4) Where do major cities fall? Together in one cluster? Or do they appear along
   more geographically divided lines?

So, the United States is basically divided into 4 county-groups. What these
clusters are, we will look at at a later date, possibly after we graph these
clusters. We'll see.
--}
