{-# LANGUAGE ViewPatterns #-}

module Data.Twitter where

-- data types for tweet representation

import Control.Arrow ((&&&))
import Data.Time hiding (parseTime)

import Control.Scan.CSV (rend)
import Data.Time.Calendar.Month (readTweetDate)

-- The indexed tweet is a tweet with a timestamp and also some index you
-- choose to provide (usually some scaled version of the idx-value of RawT)

data Tweet a = IndexedT a TimedTweet
   deriving (Eq, Ord, Show)

-- The raw tweet is complete and good for extracting from (e.g.) JSON

data RawTweet = RawT { idx, txt, created :: String, favs :: Integer }
   deriving (Eq, Ord, Show)

-- look at sample code at Graph.JSON.Cypher.Read to extract tweet from
-- JSON properties

data TimedTweet = TT {date :: Day, time :: TimeOfDay, twt :: RawTweet }
   deriving (Eq, Ord, Show)

t2tt :: RawTweet -> TimedTweet
t2tt tweet = uncurry TT (parseDateAndTime tweet) tweet

parseDateAndTime :: RawTweet -> (Day, TimeOfDay)
parseDateAndTime = (readTweetDate &&& parseTime) . created

{--
So,
*Y2016.M08.D10.Solution> let tweets = map tweet json 
*Y2016.M08.D10.Solution> created $ head tweets ~>
"Mon May 02 16:54:35 +0000 2016"
which readTweetDate handles very well.
--}

parseTime :: String -> TimeOfDay
parseTime (words -> [_,_,_,time,_,_]) = 
   let [hr,min,sec] = rend ':' time in
   TimeOfDay (read hr) (read min) (read sec)
