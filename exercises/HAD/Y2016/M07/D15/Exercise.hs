module Y2016.M07.D15.Exercise where

{--
Yesterday, we looked at a graph of @1HaskellADay tweets and saw there was 
structure there that allowed us to extract the problem and the solution for
posted on a particular day.

However, the downside, not discussed in that problem, is that the tweets,
themselves, have no information other than their twitter id, so we don't know
the dates of these problems from the tweets.

Enter Twitter API.

Okay, we are only going to scratch the surface here, because using the Twitter
API requires creating a twitter application and the, once that application is
approved by Twitter, you also need to handshake the OAuth, which is/will be 
another nightmare/learning curve for me.

So, I'm going to do some major hand-waving here (creating the twitter app and
getting and using the OAuth) and just preten my very, very simplified curl
GET-request automagically worked:

curl --get 'https://api.twitter.com/1.1/statuses/show.json'
     --data 'id=724772635016617984' --header 'Authorization: OAuth [bleh]'

... because it did.

The resulting JSON is in this directory at tweet.json or is available at the URL

https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M07/D15/tweet.json

and it is one of the tweets we examined yesterday (recall rows (head res)).

Your mission today is simple: from the JSON, build the internal representation
of that structure and then extract a Date from the tweet returned from the 
REST GET query.
--}

import Data.Aeson
import Data.Time

data Tweet = VERYdifferentStructureFromYesterday

dateFromTweet :: FilePath -> IO Day
dateFromTweet = undefined

-- given the tweet.json url/file-path, returns the date the tweet was created
-- for this exercise we don't need the time (but you can parse that if you wish)

-- HAVE AT IT!

-- Next week, we'll look at JSON from the Twitter API for a group of tweets
