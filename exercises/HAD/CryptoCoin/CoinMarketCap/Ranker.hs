module CryptoCoin.CoinMarketCap.Ranker where

{--
Hoo, boy! Sparse matrices... Sorry, dense matrices! I remember doing this
stuff with parallel FORTRAN.

We have dates (which are not guaranteed to be consecutive), indices (which are 
also not guaranteed to be consecutive) and rankings (the observed datum).

I'm thinking:

idx,date n, date n-1, date n-2, ..., date 0
1,Just 1, Just 1, ..., Nothing

like that.

This could, I suppose, be represented as a map of maps?
--}

import Control.Arrow ((&&&))

import Data.List (intercalate, replicate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time

import Control.Scan.CSV
import Control.Presentation

import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.Reporter
import CryptoCoin.CoinMarketCap.State.RankMatrix

import Data.CryptoCurrency.Types
import Data.Time.TimeSeries

{--

HERE'S A THOUGHT! ... or: ON SECOND THOUGHT!

Why don't I just CSV the ranks for now and see what external tools out there
do the job?

HUH!

... on second-second thought: nah. We still want to save our state as 
Haskell-value, and we still want to get a rank-diff of the latest.

so, ... nah on the second thought. Back to the second-second thought, which is
the first thought.
--}

-- TODO: we need a matrix module and a read (that should just be import)
-- and writer function to write out the updated matrix

matrix :: Matrix -> MetaData -> Matrix
matrix m = uncurry ansert . mkRow
   where ansert k v = Map.insert k v m

-- the thing about having keys of Days is that you can't get yesterday
-- with a pred-function. (we can do a Set.maxView, however, for today, then
-- a Set.maxView for yesterday from the remaining keysSet)

-- Okay, so we need to convert a MetaData into a (Day, RankVector), so we can
-- add that row to the matrix

mkRow :: MetaData -> (Day, RankVector)
mkRow (MetaData (Status d _ _ _ _ _) ecoins) = (d, Map.map rank ecoins)

yesterday :: Matrix -> Day -> Maybe Day -- but do we even want this?
yesterday = undefined

-- and we need to compute the rank-differential

rankDiff :: Matrix -> RankVector
rankDiff = undefined

-- but to compute rank-diff we need today and yesterday, so, we'll iterate
-- over the map-list. ... Or, ... are we only concerned about today and
-- yesterday?

rankdiff' :: RankVector -> RankVector -> RankVector
rankdiff' today yesterday =
   foldr (inserter yesterday) Map.empty (Map.toList today)

inserter :: RankVector -> (Idx, Int) -> RankVector -> RankVector
inserter yesterday (idx, rank) m =
   maybe m (\r -> Map.insert idx (rank - r) m) (Map.lookup idx yesterday)

{-- 
which further devolves into an unitary diff

adiff :: Int -> Int -> Int
adiff = (-)

and we just lift adiff into the maybe-domain.
--}

-- for analytics tools, like d3.js, mentioned below.

writeCSVfile :: FilePath -> Matrix -> IO ()
writeCSVfile = undefined

-- writing a function to write modules with functions. Macros, anyone?

writeMatrix :: FilePath -> Matrix -> IO ()
writeMatrix file =
   writeFile file
       . unlines
       . ((header:imps ++ mats) ++)
       . (++ [indent 3 ++ "]"])
       . concat
       . interleave ","
       . map showDateRankVector
       . Map.toList
      where nl = ""
            header = "module CryptoCoin.CoinMarketCap.State.RankMatrix where"
            impMatrix = "import Data.CryptoCurrency.Types (Matrix)"
            impMap = "import qualified Data.Map as Map"
            imps = [nl, impMatrix, impMap]
            decl = "rankMatrix :: Matrix"
            def = "rankMatrix = " ++ mapShow
            mats = [nl, decl, def]

showDateRankVector :: (Day, RankVector) -> [String]
showDateRankVector (d, row) =
   [indent 3 ++ ('(':showDate d) ++ (',':mapShow)]
        ++ intersperse "," (showRow (Map.toList row)) ++ [indent 6 ++ "])"]

intersperse :: String -> [String] -> [String]
intersperse _ [a] = [a]
intersperse s (h:t) = (h ++ s):intersperse s t

interleave :: String -> [[String]] -> [[String]]
interleave _ [a] = [a]
interleave s (h:t) = addToLast s h:interleave s t

addToLast :: String -> [String] -> [String]
addToLast s [a] = [a ++ s]
addToLast s (h:t) = h:addToLast s t

showDate :: Day -> String
showDate = ("read \"" ++) . (++ "\"") . show

showMap :: Show a => Show b => Map a b -> [String]
showMap = (mapShow:) . (++ [indent 3 ++ "]"]) . showRow . Map.toList

showRow :: Show a => [a] -> [String]
showRow [] = []
showRow l@(_:_) = 
   (flip (:) . showRow . drop 5
        <*> (indent 6 ++) . intercalate "," . map show . take 5) l

mapShow :: String
mapShow = "Map.fromList ["

readMatrix :: FilePath -> IO Matrix
readMatrix file = undefined

indent :: Int -> String
indent x = replicate x ' '

{--
Research:

d3.js has the following:

https://observablehq.com/@d3/bollinger-bands
https://observablehq.com/@d3/candlestick-chart
https://observablehq.com/@fil/plateau-detection?collection=@fil/interpolation
https://observablehq.com/@fil/hello-loess?collection=@fil/interpolation
https://observablehq.com/@fil/gaussian-smoothing

Do we look at all e-coins as Voronoi? or Word-cloud?

https://observablehq.com/@d3/voronoi-labels
https://observablehq.com/@d3/word-cloud
--}

{--
>>> latest
Just 2021-02-28

>>> let (Just yday) = it
>>> let tday = addDays 1 yday

>>> fetchMap $ ccmapJSON tday
...
>>> let (Just md) = it
>>> let mat = matrix rankMatrix md
>>> length mat
8
>>> take 5 . Map.toList . head $ Map.elems mat
[(1,1),(2,8),(3,584),(4,1656),(5,748)]
>>> writeMatrix "CryptoCoin/CoinMarketCap/State/RankMatrix.hs" mat

... after a reload of this module (and, subsequently, RankMatrix):

>>> latest
Just 2021-03-03

WOOT!

We really need to write the README.md here :/
--}
