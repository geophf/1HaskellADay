module Y2016.M12.D26.Exercise where 

import Data.Array
import Data.Map (Map)

-- below imports available via 1HaskellADay git repository

import Data.SAIPE.USCounties
import Graph.KMeans
import Graph.ScoreCard
import Graph.ScoreCard.Clusters

import Y2016.M12.D15.Exercise
import Y2016.M12.D21.Exercise

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
--}

partitionSAIPEBySize :: SAIPEData -> Map Size SAIPERow
partitionSAIPEBySize saipe = undefined

{--
There is a more general activity you are doing here, however, and that is,
you are partitioning any kind of data by some measure of their size. We do
have a container-type for 'any kind of measured data,' and that is the
ScoreCard.

Partition the ScoreCards of SAIPE data by size. I mean USE the SAIPE data set
but, more generally, partition ANY ScoreCard-type by Size:
--}

partitionBySize :: RealFrac c => [ScoreCard a b c] -> Map Size [ScoreCard a b c]
partitionBySize scorecards = undefined

{-- BONUS -----------------------------------------------------------------

Display your partitioned-by-size data-set using the charting tool of your
choice
--}

drawSizedPartitions :: (Show a, Show b, Show c) => Map Size [ScoreCard a b c] -> IO ()
drawSizedPartitions partitions = undefined

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

relableNodesBySize :: (Show a, Show b, Show c) => Map Size [ScoreCard a b c] -> IO ()
relableNodesBySize sizeddata = undefined

{-- MOTIVATION ------------------------------------------------------------

This relable function comes from me asking the Neo Tech folks how I could
programatically resize nodes from their attributed data. I didn't get a 
satisifactory answer. Is this, here a satifactory way programatically to
resize data nodes? We shall see, shan't we!
--}
