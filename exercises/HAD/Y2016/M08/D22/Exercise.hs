module Y2016.M08.D22.Exercise where

{--
So, we've looked at tweets, urls/links, users, and so we have just a bit left
(that concerns me, anyway), and that is the Hashtag (or '#brown' to tweeps in
the know). The structure of a hashtag is buried somewhere in these data:

"graph":
 {"nodes":
  [{"id":"1011","labels":["Hashtag"],"properties":{"name":"haskell"}},
   {"id":"255","labels":["Tweet"],
    "properties":{"id":727179491756396500,
                  "id_str":"727179491756396546",
                  "text":"April 2016 @1HaskellADay #haskell problems and solutions posted at https://t.co/QPP9j2PLsX",
                  "created_at":"Mon May 02 16:54:35 +0000 2016",
                  "favorites":0}}],
  "relationships":
  [{"id":"1549","type":"TAGS","startNode":"255","endNode":"1011","properties":{}}]}

So, yeah: the Hashtag

tease out the hashtags from the twitter graph-JSON at twitterGraphUrl
--}

import Data.Aeson
import Data.Map (Map)

-- from the git repository here:

import Data.Bag (Bag)
import Data.Twitter
import Graph.JSON.Cypher.Read.Graphs
import Graph.JSON.Cypher.Read.Tweets

import Y2016.M08.D15.Exercise (twitterGraphUrl)

data Hashtag = Tag { tag :: String } deriving (Eq, Ord, Show)

instance FromJSON Hashtag where
   parseJSON = undefined

isHashtag :: [GraphJ] -> [NodeJ]
isHashtag = undefined

hashtags :: [GraphJ] -> (String, Hashtag)
hashtags = undefined    -- gives the hashtag ids and the hashtags

-- Now find the unique set of Hashtags indexed by hashtag id:

hashtagMap :: FilePath -> IO (Map String Hashtag)
hashtagMap url = undefined

-- hint: recall that readGraphJSON twitterGraphUrl will get you [GraphJ]

-- How many hashtags are there?

{-- BONUS -----------------------------------------------------------------

A bit of a data analysis. Can you identify a User by the hashtags they use?

List the users in the data set and the hashtags they use by frequency.
--}

hashtagByUser :: [GraphJ] -> Map User (Bag Hashtag)
hashtagByUser = undefined

{--
So, something like a declarative result of:

User a uses Hashtag x 3 times, Hashtag y 7 times, ...
User b ...

Hint: ooh! the update function on the bags in the map, though ... ooh!
--}
