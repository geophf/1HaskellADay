{-# LANGUAGE OverloadedStrings #-}

module Y2016.M08.D15.Exercise where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Network.HTTP.Conduit (simpleHttp)

import Y2016.M08.D09.Solution hiding (tweet)

{--
Now for something entirely the same.

We are looking at tweets-as-graph-JSON.

So, relationships can be called out, but this fundamentally changes the 
structure of the (supporting) JSON. I've captured 100 rows of twitter data
as JSON from the following query:

match (:User { name: "1HaskellADay" })-[:POSTS]->(t:Tweet)-[r]->(n)
return t, r, n limit 100

The JSON is posted in this directory, or at this URL:
--}

graphRelUrl :: FilePath
graphRelUrl = "https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M08/D15/nodes-rels-100.json"

{--
Let's examine one of these rows of twitter data:

{"row":[{"id":727179491756396500,"id_str":"727179491756396546",
         "text":"April 2016 @1HaskellADay #haskell problems and solutions posted at https://t.co/QPP9j2PLsX",
         "created_at":"Mon May 02 16:54:35 +0000 2016","favorites":0},
        {},
        {"name":"Twitter Web Client"}],

... note that, up to now, this row is similar to the one examined before, 
except now I've excluded the user data. Okay, but now comes the following:

 "graph":{"nodes":[{"id":"1400","labels":["Source"],
                    "properties":{"name":"Twitter Web Client"}},
                   {"id":"255",
                    "labels":["Tweet"],
                    "properties":{"id":727179491756396500,
                                  "id_str":"727179491756396546",
                                  "text":"April 2016 @1HaskellADay #haskell problems and solutions posted at https://t.co/QPP9j2PLsX",
                                  "created_at":"Mon May 02 16:54:35 +0000 2016",
                                  "favorites":0}}],
          "relationships":[{"id":"3799","type":"USING","startNode":"255",
                            "endNode":"1400","properties":{}}]}},

Ugh! Nothing like repeating data we've already collected, but what, precisely,
is going on here?

The "graph"-map in the JSON (again) spells out the nodes, but not only are the
properties (again) enumerated, but also a new value "labels" is given: this is
a list of the types of the node in question.

(Under this graph, nodes (vertices) and relationships (edges) may have multiple
labels. This is 'a-okay' by the graph represented here.)

The "relationships"-map in the JSON is something new. It connects the (interal)
id-values with a relationship with its type called out specifically in the
"type"-value.

In the above JSON we have the "row" that connects the (unlabeled) nodes, as
before, but now we have a "graph" and "relationships" (meta-)data set that
explicitly calls out the node-types (as "labels") and explicitly calls out
the kinds of relationships connecting the nodes (as "type").

As the name denotes: we now have a graph-representation of our twitter data.

We also, accidentally, have small, managable ids with which to work, as the
graph data give id-values for the nodes and relationships.

So, let's scrap everything we know up to now and re-realize the JSON data
as a set of Relation-values. But not today. We will eventually get to a set
of relations of tweets in graph-data, but today, let's focus on what we can
learn from this new information.

Structure the Graph- and Relationships-types as FromJSON instances, read in
these data and then answer questions about their types/labels.
--}

data TwitterRowJ = RJ GraphJ RelationshipsJ

instance FromJSON TwitterRowJ where
   parseJSON (Object o) = RJ <$> o .: "graph" <*> o .: "relationships"

data GraphJ = YouDeclareThisType

instance FromJSON GraphJ where
   parseJSON = undefined

data RelationshipsJ = YouDeclareThatType

instance FromJSON RelationshipsJ where
   parseJSON = undefined

readTwitterData :: FilePath -> IO [TwitterRowJ]
readTwitterData = undefined

-- Okay, with that: We have tweets, that's fine, with the ["Tweet"] "label"
-- but there are also have other labeled nodes (the example above has a
-- "Source"-labeled node

-- What are all the labels for the nodes in this JSON sample found in the 
-- Graph data of the TwitterRows?

type Label = String

nodeLabels :: [TwitterRowJ] -> [Label]
nodeLabels = undefined

-- From the relationships data, what are all the "type"-values of the
-- relationships between the tweets and associated information.

relLabels :: [TwitterRowJ] -> [Label]
relLabels = undefined
