module Graph.ScoreCard where

import Control.Arrow
import Data.Array
import Data.List (transpose)

import Control.List (minmax)

{-- A solution to the problem posted at http://lpaste.net/4278240752024158208
@1HaskellADay solution for 2016-03-11

So, today, let's improve Data.ScoreCard in two ways

1. Create a ScoreCard-type that uses any type a that's an Ix-instance; and, ...

And, really? a list of (Index, values) as the vector? Why not just use a Vector,
aka Array? 

2. Do that.
--}

data ScoreCard a b val = SC { idx :: a, values :: Array b val } deriving Show

-- we care about equality of scorecards only insofar as their identity ...
-- ... I know, I know, but this is very "I don't care what is in the object"-
-- categorical, or "a vertex is a vertex"-graph, so there!

instance Eq a => Eq (ScoreCard a b c) where
   (SC a _) == (SC b _) = a == b
   (SC a _) /= (SC b _) = a /= b

{--
With this new declaration of ScoreCard, load in the shows and gaps of the
stocks listed here: http://lpaste.net/raw/3866088981561081856

(You can use the Graph.ShowsGaps.loadShowsGaps imported above to do this)

And then re-represent the ShowsGaps as ScoreCards. 

data Radix = Shows | Gaps deriving (Eq, Ord, Enum, Bounded, Ix, Show)

rerepresent :: ShowsGaps -> ScoreCard Symbol Radix Float
rerepresent (SG sym sh gp) =
   SC (read sym) (listArray (Shows, Gaps) $ map fromIntegral [sh,gp])

-- Show all the new ScoreCards. 

*Main> loadShowsGaps "stock-market/showsgaps-all.csv" ~> sg
*Main> take 3 $ map rerepresent sg ~>
[SC AA   (array (Shows,Gaps) [(Shows,33.0),(Gaps,34.0)]),
 SC AAPL (array (Shows,Gaps) [(Shows,244.0),(Gaps,10.0)]),
 SC ABB  (array (Shows,Gaps) [(Shows,2.0),(Gaps,189.0)])]
--}

-- SCALING FACTORS OF SCORECARDS ----------------------------------------------

-- actually, what we need is a scalar function that takes the bounding range
-- and returns a scaled value from that range

type Scalar a = (a, a) -> a -> a

scale :: RealFrac a => Scalar a
scale (min, max) val = (val - min) / (max - min)

-- First thing, we need to scale [ScoreCard a b c] by columns

scaleColumn :: (Ix b, RealFrac c) => b -> [ScoreCard a b c] -> [c]
scaleColumn idx = uncurry map . (scale . minmax &&& id) . map ((! idx) . values)

-- Now we do this for all the indices:

scaleScores :: (Ix b, RealFrac c) => [ScoreCard a b c] -> [ScoreCard a b c]
scaleScores scores@(SC _ v:_) =
   zipWith SC (map idx scores)
              (map (listArray (bounds v))
                   (transpose (map (`scaleColumn` scores) (indices v))))

-- map -> transpose (aka 'map') -> map -> map -> zipWith (aka 'map') ...
-- Rock-n-roll!

{--
*Main> loadScoreCardsFromEndpoint (endpoint ++ ('/': transaction)) queryTop5shows ~> scores

Previously we had six clusters, the smallest of which had ~30 members.
Let's see what happens when we bring all factors to scale:

*Main> let scaled = scaleScores scores ~> take 2 ~>
SC {idx    = AA, values = array (Count,StdDev) [(Count,0.1904762),(Min,0.0),
               (Max,0.114583336),(Mean,5.342137e-2),(StdDev,2.5309337e-2)]}
SC {idx = AAPL, values = array (Count,StdDev) [(Count,1.0),(Min,0.0),
                  (Max,3.125e-2),(Mean,6.002401e-3),(StdDev,6.2992126e-2)]}

YAY!

*Main> let (itrs, clusters) = kmeans 10 scaled ~> (id *** length) ~> (40,7) HM!
*Main> instance Show (DList a) where show = const "a DList"
*Main> clusters ~>
{(2,(Sum {getSum = 124},a DList)),
 (3,(Sum {getSum = 30},a DList)),
 (4,(Sum {getSum = 22},a DList)),
 (5,(Sum {getSum = 18},a DList)),
 (6,(Sum {getSum = 11},a DList)),
 (7,(Sum {getSum = 100},a DList)),
 (10,(Sum {getSum = 47},a DList))]

So, scaling DOES affect clustering!
--}
