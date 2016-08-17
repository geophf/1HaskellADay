module Y2016.M08.D17.Exercise where

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as Set

-- the below imported modules are available on this github repository

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Twitter
import Graph.JSON.Cypher.Read.Graphs
import Graph.JSON.Cypher.Read.Tweets

import Y2016.M08.D15.Exercise (twitterGraphUrl)

{--
So we now have tweets from graph data, but they are not connected to any of
the external information about the tweets, such as URLs and hashtags or
people tweeting or liking these tweets. The URLs and hashtags can be parsed
from the tweet-text, but this is the 'hard-way' of doing things, particularly
as twitter provides these data as nodes related to their tweets.

So, today, instead of doing a deep-dive into the related information, let's
focus on the relationships to the tweets.

The GraphJ-type has a relationships value which has (e.g.) this structure:

"relationships":[{"id":"1447",
                  "type":"USING",
                  "startNode":"255","endNode":"1000",
                  "properties":{}}]}}

and from the above import you were able to read these relationships into
Haskell-typed values. Now, let's take the next step. 

read in the graph-JSON from the twitterGraphUrl, get the unique set of tweets
and for each tweet, show the associated values (URL, user, hashtag) as raw
values (we'll parse them out later) and how these values are related to the
tweets.
--}

-- first up we want a set of indexed tweets from our graph-JSON
-- recall readGraphJSON twitterGraphUrl gives us the graph-JSON

-- note that I've generalized the Tweet-type in Data.Tweet

uniqueTweets :: [GraphJ] -> Set (Tweet String)
uniqueTweets = undefined

{-- For example:
*Y2016.M08.D17.Solution> readGraphJSON twitterGraphUrl ~> tweets
*Y2016.M08.D17.Solution> let unqt = uniqueTweets tweets ~> length ~> 29
*Y2016.M08.D17.Solution> head (Set.toList unqt)
IndexedT {index = "1134", tt = TT {date = 2016-05-20, ...}}
--}

-- use the NodeJ's idn value as the tweet index

-- Now, using the relationships derived from the graph-JSON, create a mapping
-- from (unique) tweets to id's of related data, but also include the kind
-- of the relation to that related id:

data Direction = GoingTo | ComingFrom
   deriving (Eq, Ord)

instance Show Direction where
   show GoingTo = ">"
   show ComingFrom = "<"

data Dart a = Drt Direction Label a
   deriving (Eq, Ord)

instance Show a => Show (Dart a) where
   show (Drt GoingTo lbl val) = ' ':lbl ++ "> " ++ show val
   show (Drt ComingFrom lbl val) = " <" ++ lbl ++ " " ++ show val

relatedData :: Set (Tweet String) -> [GraphJ]
            -> MultiMap (Tweet String) (Dart String) (Set (Dart String))
relatedData = undefined

{--
For example:
*Y2016.M08.D17.Solution> let reld = relatedData unqt tweets ~> head . MM.toList ~>
(IndexedT {index = "1134", tt = TT {date = 2016-05-20,...}},
 fromList [CONTAINS> "1494", REPLY_TO> "1241", TAGS> "1011", USING> "1400"])
--}
