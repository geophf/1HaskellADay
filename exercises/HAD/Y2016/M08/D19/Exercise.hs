module Y2016.M08.D19.Exercise where

import Data.Aeson
import Data.Set (Set)

import Data.MultiMap (MultiMap)
import Data.Twitter
import Graph.JSON.Cypher.Read.Graphs
import Graph.JSON.Cypher.Read.Tweets

import Y2016.M08.D15.Exercise (twitterGraphUrl)

{--
So, yesterday we extracted URLs from twitter graph-JSON.

Today we'll extract users, or, in twitter-parlance: tweeps.

From the twitter graph-data at twitterGraphUrl, extract the users. The user
node data has the following structure:

{"id":"504",
 "labels":["User"],
 "properties":{"screen_name":"1HaskellADay",
               "name":"1HaskellADay",
               "location":"U.S.A.",
               "followers":1911,
               "following":304,
               "profile_image_url":"http://pbs.twimg.com/profile_images/437994037543309312/zkQSpXnp_normal.jpeg"}

Reify these data into a Haskell-type
--}

data User = YourRepresentationOfATwitterUser

instance FromJSON User where
   parseJSON = undefined

readTwitterUsers :: FilePath -> IO [User]
readTwitterUsers = undefined

-- recall that readGraphJSON twitterGraphUrl gets us the twitter data

-- How many unique users are found in this data-set?

uniqueUsers :: [GraphJ] -> Set User
uniqueUsers = undefined

-- hint: define uniqueUsers to define readTwitterUsers

{-- BONUS -----------------------------------------------------------------

From Read.Tweets we can get [GraphJ] of the nodes and relations. Answer the
below:

What is the distribution of tweets to users?
--}

userTweets :: [GraphJ] -> MultiMap User (Tweet String) (Set (Tweet String))
userTweets = undefined

-- hint: recall that we related tweets to ids in Y2016.M08.D17.Exercise
-- hint: recall we extracted a set of unique tweets from graph-JSON in
--       Graph.JSON.Cypher.Read.Tweets
