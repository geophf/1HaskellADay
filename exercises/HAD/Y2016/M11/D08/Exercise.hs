module Y2016.M11.D08.Exercise where

import Data.Aeson

{--
So, last week we looked at a graph of twitter-users. NOICE! If you look at the
source code that generated the graph, it's a python script that makes a 
request to the twitter API to get a list of the followers of the graphed
account.

Well. I happen to have a twitter API account and twitter application. Why don't
I just access these data myself and then we can work on the raw twitter (JSON)
data themselves.

Indeed!

So, today's Haskell exercise. In this directory, or at the URL:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M11/D08/1haskfollowersids.json.gz

is a gzipped list of twitter ids that follow 1HaskellADay as JSON.

I was able to get this JSON data with the following REST GET query:

https://api.twitter.com/1.1/followers/ids?screen_name=1HaskellADay

via my (o)authenticated twitter application.

Read in the JSON (after gunzipping it) and answer the below questions
--}

type TwitterId = Integer

data Tweeps = TIds { tweeps :: [TwitterId] } deriving Show

instance FromJSON Tweeps where
   parseJSON = undefined

followers :: FilePath -> IO Tweeps
followers json = undefined

-- 1. How many followers does 1HaskellADay have?

-- 2. What is the max TwitterID? Is it an Int-value? (ooh, tricky)

-- 3. What is the min TwitterID?

-- Don't answer this:
-- 4. Trick question: what is 1HaskellADay's twitter ID?

{--
We'll be looking at how to get screen_name from twitter id and twitter id from
screen_name throughout this week, as well as looking at social networks of
followers who follow follwers who ...

How shall we do that? The Twitter API allows summary queries, as the JSON
examined here, as well as in-depth queries, given a screen_name (from which you
can obtain the twitter ID) or a twitter id (from which you can obtain the
screen_name) and followers, as you saw in today's query.
--}
