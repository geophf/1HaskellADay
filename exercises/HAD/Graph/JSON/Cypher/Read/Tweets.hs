{-# LANGUAGE OverloadedStrings #-}

module Graph.JSON.Cypher.Read.Tweets where

-- provides functions for extracting tweets from graph-JSON

import Control.Arrow ((&&&), (>>>))
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Twitter
import Graph.JSON.Cypher.Read
import Graph.JSON.Cypher.Read.Graphs

{--
tweetFrom :: PropertiesJ -> Maybe RawTweet
tweetFrom (PJ props) = 
   RawT <$> props <<-$ "id_str" <*> props <<-$ "text"
        <*> props <<-$ "created_at" <*> props <<-# "favorites"

-- problem: not all tweets have these fields: some are just bare id's
-- I chose to reject these place-holders for now.

*Y2016.M08.D16.Solution> readGraphJSON twitterGraphUrl ~> tweets
*Y2016.M08.D16.Solution> let twit = last . nodes $ head tweets
*Y2016.M08.D16.Solution> twit ~> 
NJ {idn = "255",labels = ["Tweet"],propsn = PJ (fromList [("created_at",...)])}
*Y2016.M08.D16.Solution> tweetFrom (propsn twit) ~>
Tweet {idx = "727179491756396546", txt = "April 2016 @1HaskellADay...", ...}
--}

instance FromJSON RawTweet where
   parseJSON (Object o) = RawT <$> o .: "id_str" <*> o .: "text"
         <*> o .: "created_at" <*> o .: "favorites"

-- Using the above declaration, define the below

-- first, we need to know which nodes are tweets:

isTweet :: NodeJ -> Bool
isTweet = elem "Tweet" . labels

-- And then we get all nodes from the graph that are tweets

filterTweetNodes :: [GraphJ] -> [NodeJ]
filterTweetNodes = filter isTweet . concatMap nodes

-- okay, for the indexed tweets, we now no longer need to determine an index
-- as the cypher query result generates one for its internal use and shares it
-- with us! Convenient.

indexedTweets :: [GraphJ] -> Map String RawTweet
indexedTweets = 
   filterTweetNodes                             >>>
   mapMaybe (sequence . (idn &&& node2valM))    >>> 
   Map.fromList

{-- 
How many unique tweets are in the data set?
*Y2016.M08.D16.Solution> let idxt = indexedTweets tweets
*Y2016.M08.D16.Solution> length idxt ~> 33
--}

-- And with all the above we have:

readTweetsFrom :: FilePath -> IO [TimedTweet]
readTweetsFrom = fmap tweetsFrom . readGraphJSON

tweetsFrom :: [GraphJ] -> [TimedTweet]
tweetsFrom = map t2tt . Map.elems . indexedTweets

{--
*Y2016.M08.D16.Solution> fmap head (readTweetsFrom twitterGraphUrl) ~>
TT {date = 2016-05-20, time = 18:32:47, 
    twt = Tweet {idx = "733727186679672833", 
                 txt = "The weight's the thing\nWherein I'll catch the "
                    ++ "conscience of the King\n... no ... weight ...\nToday's "
                    ++ "#haskell solution https://t.co/XVqbPRfjAo",
                 created = "Fri May 20 18:32:47 +0000 2016",
                 favs = 1}}
--}

-- we want a set of indexed tweets from our graph-JSON

uniqueTweets :: [GraphJ] -> Set (Tweet String)
uniqueTweets = 
   indexedTweets           >>>
   Map.map t2tt            >>>
   Map.toList              >>>
   map (uncurry IndexedT)  >>>
   Set.fromList

{--
*Y2016.M08.D17.Solution> readGraphJSON twitterGraphUrl ~> tweets
*Y2016.M08.D17.Solution> let unqt = uniqueTweets tweets ~> length ~> 29
*Y2016.M08.D17.Solution> head (Set.toList unqt) ~>
IndexedT {index = "1134", tt = TT {date = 2016-05-20, ...}}
--}

-- URL -----------------------------------------------------------------

instance FromJSON URL where
   parseJSON (Object o) = URI <$> o .: "url"

twitterURLs :: [GraphJ] -> Map String URL
{--
twitterURLs = Map.fromList . mapMaybe (sequence . (idn &&& node2valM))
            . filter ((elem "Link") . labels) . concatMap nodes
--}
twitterURLs = nodeMapper "Link"   -- MUCH better!

-- notice how twitterURLs follows the same template as indexedTweets

-- so:

nodeMapper :: FromJSON a => String -> [GraphJ] -> Map String a
nodeMapper k = Map.fromList . mapMaybe (sequence . (idn &&& node2valM))
             . filter ((elem k) . labels) . concatMap nodes
