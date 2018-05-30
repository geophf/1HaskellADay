{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2018.M05.D30.Solution where

{--
Today we're going to look at big(ish) data. What do you do with big(ish) data?

Well, 99% you ignore, AND WE WILL FOLLOW THAT FINE TRADITION TODAY!

Today's Haskell problem. We have a JSON of MASSIVE proporations (well, a mega-
byte, but we can pretend, no?), of structure below ... that is to say, a LOT
of the structure we just don't care about, but we do care about the id sets
and the time it took to run the process on each element (below).

So, parse the JSON, then return the following buckets: under 10 seconds, 10-20
seconds, 20-30 seconds, and 30+ seconds.
--}

import Control.Arrow ((&&&))

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sortOn, groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Article a = Art { idx :: Integer, uuid :: String, entities :: [a], time :: Double }
   deriving (Eq, Ord, Show)

-- the JSON is pretty-printed into this file

exDir :: FilePath
exDir = "Y2018/M05/D30/"

entitiesJSON :: FilePath
entitiesJSON = "pilot_entities.json"

-- parse in Article Value values from the JSON:

readArts :: FilePath -> IO [Article Value]
readArts = fmap (fromMaybe [] . decode) . BL.readFile

-- but, of course, you need a JSON instance to do that:

instance FromJSON a => FromJSON (Article a) where
   parseJSON (Object o) = Art <$> o .: "index" <*> o .: "uuid"
                              <*> o .: "entities" <*> o .: "time"

-- okay, now put them into your bucket list:

data Bucket = SMALL | TALL | GRANDE | VENTI
   deriving (Eq, Ord, Show)

bucket :: Article a -> Bucket
bucket (time -> art) | art < 10 = SMALL
                     | art < 20 = TALL
                     | art < 30 = GRANDE
                     | otherwise = VENTI
      
{--
where SMALL < 10 seconds
      TALL 10 sec to < 20 seconds
      GRANDE 20 seconds < 30 seconds
  and VENTI 30+ seconds
--}

type BucketList a = Map Bucket [Article a]

bucketList :: [Article a] -> BucketList a
bucketList =
   Map.fromList . map (fst . head &&& map snd)
      . groupBy ((==) `on` fst) . sortOn fst . map (bucket &&& id)

-- maybe use a multimap?

-- How many articles are in each bucket?
-- What is the mean time of computing the entities from an article text?
-- That is: what is the mean of all (time art)?

{--
>>> arts <- readArts (exDir ++ entitiesJSON)
>>> length arts
1000

>>> Map.map length (bucketList arts)
fromList [(SMALL,798),(TALL,167),(GRANDE,17),(VENTI,18)]

>>> sum (map time arts) / 1000
8.274935276031494
--}

{-- BONUS -----------------------------------------------------------------

Using whatever charting tool you like, chart your results
--}

chartBucketList :: BucketList a -> IO ()
chartBucketList = -- I just put this into a CSV file an chart in on Numbers
   mapM_ putStrLn . ("size,count":)
    . map (\(a,b) -> show a ++ (',':b)) . Map.toList . Map.map (show . length)

{--
>>> chartBucketList (bucketList arts)
size,count
SMALL,798
TALL,167
GRANDE,17
VENTI,18
--}
