{-# LANGUAGE ViewPatterns #-}

module Y2016.M08.D10.Solution where

import Control.Arrow (first, (&&&), (>>>))
import Control.Monad.State (execState)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time hiding (parseTime)
import Data.Time.LocalTime

import Control.Scan.CSV (rend)
import Data.SymbolTable (SymbolTable)
import Data.Time.Calendar.Month (readTweetDate)
import qualified Data.SymbolTable as SymT
import Graph.JSON.Cypher

import Y2016.M08.D08.Solution (url, readJSONRows)
import Y2016.M08.D09.Solution

indexedTweets :: [TableRow] -> Map Int Tweet
indexedTweets =
   map tweet                                                        >>>
   ((`execState` SymT.empty) . mapM (SymT.fromEnumS . idx)  &&& id) >>>
   uncurry map . first (\st -> SymT.intVal st . idx &&& id)         >>>
   Map.fromList

-- Hint: since this slice is small-data, one way to index by the idx is to put 
-- those values into a SymbolTable, then read out their enumerated values.

-- recall that we read in the rows of JSON with: readJSONRows url

{--
*Y2016.M08.D10.Solution> readJSONRows url ~> json
*Y2016.M08.D10.Solution> let idxtw = indexedTweets json
*Y2016.M08.D10.Solution> length idxtw ~> 23
--}

-- Use the above definition to answer the below questions.

-- We have 100 rows. How many unique tweets do we have in this data selection?

uniqueTweets :: [TableRow] -> Int
uniqueTweets = length . indexedTweets

-- *Y2016.M08.D10.Solution> uniqueTweets json ~> 23

{-- BONUS -----------------------------------------------------------------

Another problem is that the JSON does not have a concept of time, so the
created field is a string. Rerealize the tweets to have Day and TimeOfDay
fields (which requires parsing the created field string
--}

data TimedTweet = TT {date :: Day, time :: TimeOfDay, twt :: Tweet }
   deriving (Eq, Ord, Show)

t2tt :: Tweet -> TimedTweet
t2tt tweet = uncurry TT (parseDateAndTime tweet) tweet

parseDateAndTime :: Tweet -> (Day, TimeOfDay)
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

{--
*Y2016.M08.D10.Solution> t2tt $ head tweets ~>
TT 2016-05-02 16:54:35 (Tweet {idx = "727179491756396546", ...})
--}

-- With this definition, answer the below:

-- How many tweets, on average, are tweeted each day?

tweetsPerDay :: [TableRow] -> Float
tweetsPerDay = µ . map (fromIntegral . length) . tweetsByDay

µ :: RealFrac a => [a] -> a
µ = sum &&& fromIntegral . length >>> uncurry (/)

timestampedTweets :: [TableRow] -> [TimedTweet]
timestampedTweets = map t2tt . Map.elems . indexedTweets

tweetsByDay :: [TableRow] -> [[TimedTweet]]
tweetsByDay = groupBy ((==) `on` date) . sort . timestampedTweets

-- *Y2016.M08.D10.Solution> tweetsPerDay json ~> 1.7692307

-- How many days are covered (spanned) in this data set?

daysOfOurTweets :: [TableRow] -> Integer
daysOfOurTweets = uncurry (flip diffDays) . tweetsSpanning

-- *Y2016.M08.D10.Solution> daysOfOurTweets json ~> 123

-- What is the start and end date of the days covered?

tweetsSpanning :: [TableRow] -> (Day, Day)
tweetsSpanning = (head &&& last) . tweetingDays

tweetingDays :: [TableRow] -> [Day]
tweetingDays = map (date . head) . tweetsByDay

{--
*Y2016.M08.D10.Solution> tweetingDays json ~> [2016-03-31,2016-05-02,...]
*Y2016.M08.D10.Solution> tweetsSpanning json ~> (2016-03-31,2016-08-01)
--}
