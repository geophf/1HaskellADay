module Y2016.M08.D09.Exercise where

import Y2016.M08.D08.Exercise

import Data.Aeson
import Graph.JSON.Cypher.Read.Rows

{--
Okay, yesterday we saw we had 100 rows! YAY! of unstructured data. YAY!

... kinda.

Let's start putting some structure to these rows of data.

Well, we saw that each row was an array of (an array of) 5 elements. Let's
look at one of these rows again.

(from yesterday:)
*Y2016.M08.D08.Solution> (length &&& id) $ rowElements (head json) ~>
(5,

Object (fromList [("screen_name",String "1HaskellADay"),       -- 1
                  ("profile_image_url",String "http://pbs.twimg.com/profile_images/437994037543309312/zkQSpXnp_normal.jpeg"),
                  ("location",String "U.S.A."),
                  ("following",Number 304.0),
                  ("name",String "1HaskellADay"),
                  ("followers",Number 1906.0)]))

Object (fromList [])                                           -- 2

Object (fromList [("favorites",Number 0.0),                    -- 3
                  ("id_str",String "727179491756396546"),
                  ("text",String "April 2016 @1HaskellADay #haskell problems and solutions posted at https://t.co/QPP9j2PLsX"),
                  ("created_at",String "Mon May 02 16:54:35 +0000 2016"),
                  ("id",Number 7.271794917563965e17)])

Object (fromList [])                                           -- 4

Object (fromList [("name",String "<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>")
                                                               -- 5

Okay, that.

Objects 2 and 4 are relations, and it would be great to encode the type of 
relation (I just tweeted that request to @neo4j, so that update will come RSN!),
but we don't have that luxury in this JSON, so we'll ignore 2 and 4 for now.

Object 1. Again, no type, so let's capitulate and examine the query that 
generated the JSON for some structural information:

match tweet=(u:User { name: '1HaskellADay' })-[:POSTS]->(t:Tweet)-[]->(n) 
return tweet limit 100

Ah, okay! And as we see as we examine each row, Object 1 is always the same.

Let's ignore object 1 for now, then.

Object 3 and 5 remain. Object 5 varies, depending on what (type) the tweet
is related to, so let's put off looking at object 5 for now.

This leaves object 3.

Object 3 is the tweet.
--}

data Tweet = ChirpSomethingOrOther

{--
Declare the structure of a tweet based on the structure you observe from the
rows of data returned to you from yesterday's exercise.

Now. Write a FromJSON instance for the Tweet-type and extract the tweets
from the rows you've received.
--}

instance FromJSON Tweet where
   parseJSON = undefined

tweet :: TableRow -> Tweet
tweet = undefined

-- Hint: rowElements from yesterday's exercise might help you get to the 
-- element that's the tweet data.

-- recall that the url is defined in yesterday's exercise (imported above).

-- with the above defined, how many tweets contain the word '#haskell' or
-- '#Haskell'? All of them?

tweetsContainingHaskellTag :: [Tweet] -> Int
tweetsContainingHaskellTag = undefined

-- What are the tweets that do NOT have the #haskell/#Haskell hashtag?

noHaskellNahCry :: [Tweet] -> [Tweet]
noHaskellNahCry = undefined

{--
Now there are other ways to search for tweets associated with a hashtag, but
that involved Object 5, and we will not look at that today.

Also, do you see these id's for these tweets? We will (not today) look at
providing a scale on these id's. Also, search by time? The JSON (nor Cypher,
for that matter) has no concept of dates, but Haskell has that data-type. We'll
look at classifying tweets by dates another day as well.
--}
