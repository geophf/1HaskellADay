module Y2016.M08.D10.Exercise where

import Data.Map (Map)
import Data.Time
import Data.Time.LocalTime

import Data.SymbolTable
import Graph.JSON.Cypher

import Y2016.M08.D08.Exercise (url, readJSONRows)
import Y2016.M08.D09.Exercise

{--
So, yesterday we were able to dig the tweets from graph JSON and look at the
text of these tweets.

An observation: the tweets' identities are so large that precision is lost
if you use the 'Number'-type as an identifier. Confirm this for yourself!

So we're 'stuck' with using the stringified version of these ids as keys for
these tweets

Or. Are. We?

This is the big data-problem. The number of tweets in the world is so large
that the ones we're looking for get lost in the sea of identifying them, even
as we are examining the very tweets we care about.

Something must be done about this, immediately! immediately! Harrumph! Harrumph!

Create a key-scheme/labeling-scheme/identifying-scheme for these tweets, then
map the labels to their respective tweets.
--}

indexedTweets :: Ord a => [TableRow] -> Map a Tweet
indexedTweets = undefined

-- Hint: since this slice is small-data, one way to index by the idx is to put 
-- those values into a SymbolTable, then read out their enumerated values.

-- recall that we read in the rows of JSON with: readJSONRows url

-- Use the above definition to answer the below questions.

-- We have 100 rows. How many unique tweets do we have in this data selection?

uniqueTweets :: [TableRow] -> Int
uniqueTweets = undefined

{-- BONUS -----------------------------------------------------------------

Another problem is that the JSON does not have a concept of time, so the
created field is a string. Rerealize the tweets to have Day and TimeOfDay
fields (which requires parsing the created field string
--}

data TimedTweet = TT Day TimeOfDay Tweet 

t2tt :: Tweet -> TimedTweet
t2tt = undefined

-- With this definition, answer the below:

-- How many tweets, on average, are tweeted each day?

tweetsPerDay :: [TableRow] -> Float
tweetsPerDay = undefined

-- Hint: consider Data.List.groupBy on sorted TimedTweets

-- How many days are covered (spanned) in this data set?

daysOfOurTweets :: [TableRow] -> Int
daysOfOurTweets = undefined

-- What is the start and end date of the days covered?

tweetsSpanning :: [TableRow] -> (Day, Day)
tweetsSpanning = undefined

-- Hint: use tweetsSpanning to answer daysOfOurTweets
