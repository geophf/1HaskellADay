module Y2016.M08.D16.Exercise where

{--
Now that we have graph information, let's start resolving these NodeJ-values
into more specific types.

From the import above, read in the JSON-as-graphs and from the nodes of that
graph return all the tweets-as-TimedTweets.
--}

import Network.HTTP.Conduit (simpleHttp)

import Graph.JSON.Cypher.Read.Graphs
import Y2016.M08.D09.Exercise hiding (tweet)
import Y2016.M08.D10.Exercise
import Y2016.M08.D15.Exercise (twitterGraphUrl)

tweetFrom :: PropertiesJ -> Tweet
tweetFrom props = undefined

-- Hint: filter NodeJ-values that have a "Tweet" label, realize a Tweet-value
-- from the PropertiesJ, then convert that to a TimedTweet using t2tt.

-- Use readGraphJSON to read in the tweets from twitterGraphUrl

-- Using the above definition, define the below

tweetsFrom :: FilePath -> IO [TimedTweet]
tweetsFrom url = undefined

-- How many unique tweets are in the data set?

-- We'll look at the nodes related to the tweets (User, Link, Source)
-- throughout the rest of this week
