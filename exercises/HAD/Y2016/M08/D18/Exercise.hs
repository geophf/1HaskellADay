module Y2016.M08.D18.Exercise where

import Data.Aeson

import Data.Relation
import Graph.JSON.Cypher.Read.Graphs

import Y2016.M08.D15.Exercise (twitterGraphUrl)

{--
Yesterday, we've delevoped labeled, directional relations: darts and then
related those darts to the originating tweets.

Great.

But all those related data were just related by id yesterday. That's not very
helpful for data mining, if you want to see more than just the structure of
your social network.

So, today, we'll expand the domain of known types in our twitterverse.

Interesting pieces are links (URLs), hashtags, and people.

Let's start with links.

From the twitter graph-JSON, available at twitterGraphUrl, scan through the
nodes and identify the links/URLs in those data, extract them and put them
into a Haskell-friendly type.

The structure of an URL in JSON in the twitter nodes-set is:

{"url":"http://logicaltypes.blogspot.com/2016/05/april-2016-1haskelladay-problem-and.html"}
--}

data URL = URI { url :: String } deriving (Eq, Ord, Show

instance FromJSON URL where
   parseJSON = undefined

readTwitterURLs :: FilePath -> IO [URL]
readTwitterURLs = undefined

-- hint: the URLs' information(s) are in the nodes of the GraphJ of the
-- twitter graph-JSON

-- reminder that readGraphJSON twitterGraphUrl will get you the [GraphJ]

-- Question: how many URLs are in this graph-JSON
