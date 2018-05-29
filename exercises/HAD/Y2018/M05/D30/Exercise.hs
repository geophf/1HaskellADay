{-# LANGUAGE OverloadedStrings #-}

module Y2018.M05.D30.Exercise where

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

import Data.Aeson
import Data.Map (Map)

data Article a = Art { idx :: Integer, uuid :: String, entities :: [a], time :: Double }
   deriving (Eq, Ord, Show)

-- the JSON is pretty-printed into this file

exDir :: FilePath
exDir = "Y2018/M05/D30/"

entitiesJSON :: FilePath
entitiesJSON = "pilot_entities.json"

-- parse in Article Value values from the JSON:

readArts :: FilePath -> IO [Article Value]
readArts file = undefined

-- but, of course, you need a JSON instance to do that:

instance FromJSON a => FromJSON (Article a) where
   parseJSON (Object o) = undefined

-- okay, now put them into your bucket list:

data Bucket = SMALL | TALL | GRANDE | VENTI
   deriving (Eq, Ord, Show)

{--
where SMALL < 10 seconds
      TALL 10 sec to < 20 seconds
      GRANDE 20 seconds < 30 seconds
  and VENTI 30+ seconds
--}

type BucketList a = Map Bucket [Article a]

bucketList :: [Article a] -> BucketList a
bucketList arts = undefined 

-- maybe use a multimap?

-- How many articles are in each bucket?
-- What is the mean time of computing the entities from an article text?
-- That is: what is the mean of all (time art)?

-- Which article took the longest to process?

{-- BONUS -----------------------------------------------------------------

Using whatever charting tool you like, chart your results
--}

chartBucketList :: BucketList a -> IO ()
chartBucketList bins = undefined

-- there's d3, excel, Haskell libraries, ... HAVE AT IT!
