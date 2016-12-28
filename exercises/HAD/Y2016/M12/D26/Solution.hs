module Y2016.M12.D26.Solution where 

import Control.Comonad ((=>>), extract)
import Control.Monad (void)

import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map

import System.Environment (getEnv)

-- below imports available via 1HaskellADay git repository

import Analytics.Math.Statistics.StandardDeviation

import Control.DList
import Control.List -- for Comonad on List

import Data.SAIPE.USCounties

import Graph.KMeans
import Graph.ScoreCard
import Graph.ScoreCard.Clusters

import Y2016.M12.D15.Solution
import Y2016.M12.D21.Solution

{--
Good morning, all. Happy Boxing Day!

So, last week we clustered SAIPE/poverty data for Counties of the US, and saw 
some interesting things that we'll explore later this week, and associate back 
to US States. But that later.

The K-means algorithm is a good classifier, no doubt; I mean, it's way better 
than going through a spreadsheet, row-by-row...

... and guess how most companies look at their data-sets?

Yup, in a spreadsheet, row-by-row.

But K-means is not the only way to look at a data-set, by no means. It's a
good one, but not the only one, and issues with k-means, particularly the time-
cost of classification as the data-sets get larger, as you saw when you 
classified the SAIPE-data. That took a bit of time to do.

So, other forms of classification? What are your recommendations, Haskellers?
Tweet me and we can look at those.

For today, we're going to do an eh-classification. That is to say, we're going
to use the old eyeballs or our seats-of-the-pants method of classification to
divvy up these data into one of five categories:
--}

data Size = Tiny | Small | Medium | Large | YUGE
   deriving (Eq, Ord, Enum, Bounded, Ix, Show, Read)

{--
Today's Haskell problem: take the data at

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

read it in, and, using a classification method of your choice, partition
these data in to the groups of your choosing. How do divvy up these data?
Well, you have population, you have poverty, and you have the ratio of the
two, don't you? So, you choose which attribute you use to partition the data.

partitionSAIPEBySize :: SAIPEData -> Map Size SAIPERow
partitionSAIPEBySize saipe = undefined

There is a more general activity you are doing here, however, and that is,
you are partitioning any kind of data by some measure of their size. We do
have a container-type for 'any kind of measured data,' and that is the
ScoreCard.

Partition the ScoreCards of SAIPE data by size. I mean USE the SAIPE data set
but, more generally, partition ANY ScoreCard-type by Size:
--}

partitionBySize :: (Ix b, RealFrac c) => [ScoreCard a b c] -> SCClusters Size a b c
partitionBySize = foldr (uncurry add1) Map.empty . marrySize

marrySize :: (Ix b, RealFrac c) => [ScoreCard a b c] -> [(Size, ScoreCard a b c)]
marrySize = zip . map sizeFromσ . σs <*> id

{--
*Y2016.M12.D26.Solution> let parts = partitionBySize scorecards ~>
fromList [(Medium,(Sum {getSum = 2981},a DList)),
          (Large,(Sum {getSum = 79},a DList)),
          (YUGE,(Sum {getSum = 82},a DList))]
--}

-- this falls in very nicely with standard deviations, don't you think?

-- first, let's compute the centroid for these scorecards:

centroid :: (Ix b, RealFrac c) => ScoreCard a b c -> c
centroid = fromRational . rsqte . sum . map ((^2) . toRational) . elems . values

-- hm. A thought: I wonder if embedding the centroid into the scorecard will
-- speed up the kmeans clustering.

{--
-- Okay, now that we have each scorecard's centroid, lets compute the mean, µ,
-- and the variance for a set of scorecards, then compute the variance from µ

type Variances c = (c, [(c, c)])

variances :: (Ix b, RealFrac c) => [ScoreCard a b c] -> Variances c
variances scorecards = 
   let centroids = map centroid scorecards
       mean      = µ centroids
   in  (mean, zip centroids (map var (zip centroids (repeat mean))))

*Y2016.M12.D26.Solution> readSAIPERaw "Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz" ~> raw
*Y2016.M12.D26.Solution> let scorecards = saipeRows2SC raw
*Y2016.M12.D26.Solution> head scorecards 
SC {idx = Autauga County (AL), values = [(Population,54860.0),(Poverty,6966.0)]}
*Y2016.M12.D26.Solution> centroid (head scorecards) ~> 55300.5

*Y2016.M12.D26.Solution> µ (map centroid scorecards) ~> 100977.15

This is the mean of our scorecards centroids

*Y2016.M12.D26.Solution> let (mean, vars) = variances scorecards 
*Y2016.M12.D26.Solution> head vars ~> (55300.5,2.0863562e9)
*Y2016.M12.D26.Solution> rsqte . toRational . fst $ head vars ~> 776503 % 17
*Y2016.M12.D26.Solution> floor it ~> 45676

Now let's compute the σ

So, we'll take the variances vis-à-vis the mean to get σ at each centroid

σs :: RealFrac c => Variances c -> [StandardDeviation]
σs (µ, centVars) = 
   let context = zip (map snd centVars) (repeat µ) in
   context =>> σ context . extract

Actually, no: σ computes variance. All I need is mean and centroid
--}

data Side = Hi | Lo deriving (Eq, Ord, Show)

σs :: (Ix b, RealFrac c) => [ScoreCard a b c] -> [(StandardDeviation, Side)]
σs scorecards =
   let centres = map centroid scorecards
       mean    = µ centres
       context = zip centres (repeat mean)
   in  zip (context =>> σ context . extract) (map (hilo mean) centres)

hilo :: RealFrac c => c -> c -> Side
hilo a b = if a > b then Lo else Hi

{--
*Y2016.M12.D26.Solution> let siggies = σs scorecards 
*Y2016.M12.D26.Solution> mapM_ print (take 10 siggies)
(SD One 0.13,Lo)
(SD One 0.31,Hi)
(SD One 0.23,Lo)
(SD One 0.24,Lo)
(SD One 0.13,Lo)
(SD One 0.27,Lo)
(SD One 0.24,Lo)
(SD One 0.04,Hi)
(SD One 0.20,Lo)
(SD One 0.22,Lo)

And with this, I can compute the Size value
--}

sizeFromσ :: (StandardDeviation, Side) -> Size
sizeFromσ (SD One _, _) = Medium
sizeFromσ (SD Two _, Hi) = Large
sizeFromσ (SD Two _, Lo) = Small
sizeFromσ (SD ThreeOn _, Hi) = YUGE
sizeFromσ (SD ThreeOn _, Lo) = Tiny

{--
*Y2016.M12.D26.Solution> let sizes = map sizeFromσ siggies 
*Y2016.M12.D26.Solution> let extras = filter (/= Medium) sizes
*Y2016.M12.D26.Solution> length extras ~> 161

for 3000 rows, that looks about right.

*Y2016.M12.D26.Solution> mapM_ print (take 10 extras)
Large
YUGE
YUGE
YUGE
YUGE
YUGE
YUGE
YUGE
YUGE
YUGE

Some YUGE counties out there.

*Y2016.M12.D26.Solution> let smallies = filter (< Large) extras
*Y2016.M12.D26.Solution> length smallies ~> 0

Urg! That's a problem, as I know there's at least 1 county in Hawaii that has
zero population. Let's just go with it for now.
--}

{-- BONUS -----------------------------------------------------------------

Display your partitioned-by-size data-set using the charting tool of your
choice
--}

drawSizedPartitions :: (Show a, Show b, Show c, RealFrac c, Enum a, Ix b, Ix a) =>
                       SCClusters Size a b c -> [ScoreCard a b c] -> IO ()
drawSizedPartitions partitions scorecards = getEnv "CYPHERDB_ACCESS" >>=
   let colors = colorization partitions scorecards
       rels   = relateClusters (NC ("US Counties", colors)) in
   void . flip uploadClusters rels

{--
*Y2016.M12.D26.Solution> drawSizedPartitions parts scorecards 

... A sample view is included in this directory showing the YUGE counties
(New Haven, CT being one of them) and the Large counties. The remaining counties
(all 2900+ of them) are classified as 'medium' in size.
--}

{-- HINT ------------------------------------------------------------------

You can use the Clusters-type, you know. kmeans doesn't own that type, and
each cluster is identified by its corresponding Size-value. If you reorganize
the Map Size [ScoreCard a b c] as Graph.KMeans.Clusters value with a little
Map.map-magic, then the resulting value draws itself, as you saw from the
solution from the Y2016.M12.D23 solution.
--}

{-- BONUS-BONUS -----------------------------------------------------------

Now that we have size information in the graphed data set, re-lable each of
the Cell nodes with Size information, so that, e.g.: small nodes show visually
as small and YUGE nodes show as YUGE!

YUGE, adj.: means YUGE, ICYMI
--}

relableNodesBySize :: (Show a, Show b, Show c) => SCClusters Size a b c -> IO ()
relableNodesBySize sizeddata = undefined

{-- MOTIVATION ------------------------------------------------------------

This relable function comes from me asking the Neo Tech folks how I could
programatically resize nodes from their attributed data. I didn't get a 
satisifactory answer. Is this, here a satifactory way programatically to
resize data nodes? We shall see, shan't we!

... this particular example is so easily classified by size that showing it
visually would be a distraction IMO, so ... moving on.
--}
