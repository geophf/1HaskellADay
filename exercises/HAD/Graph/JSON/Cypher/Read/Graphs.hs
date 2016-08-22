{-# LANGUAGE OverloadedStrings #-}

module Graph.JSON.Cypher.Read.Graphs where

{--
When a Cypher query-result comes back with rows AND graphs, we have here a way
to parse the graph information into Haskell-representations of nodes and 
relationships
--}

import Control.Arrow ((&&&))
import Data.Aeson
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Network.HTTP.Conduit (simpleHttp)

{--
The graph-structure returned is as follows, e.g.:

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
--}

data GraphDataJ = GDJ { rows :: [RowGraphJ] } deriving Show

instance FromJSON GraphDataJ where
   parseJSON (Object o) = GDJ <$> o .: "data"

data RowGraphJ = RGJ { graph :: GraphJ } deriving Show

instance FromJSON RowGraphJ where
   parseJSON (Object o) = RGJ <$> o .: "graph"

data GraphJ = GJ { nodes :: [NodeJ], rels :: [RelJ] } deriving Show

instance FromJSON GraphJ where
   parseJSON (Object o) = GJ <$> o .: "nodes" <*> o .: "relationships"

data PropertiesJ = PJ (Map String Value) deriving Show

instance FromJSON PropertiesJ where
   parseJSON (Object o) = PJ <$> o .: "properties"

-- sometimes I need properties-lists as objects:

prop2obj :: PropertiesJ -> Value
prop2obj (PJ o) = toJSON o

-- and I need to parse that into Results. The thing is, I don't like Result
-- values, but I'm fine with Maybe values, so we have this (lossy) translation:

res2mb :: Result a -> Maybe a
res2mb (Success a) = Just a
res2mb _           = Nothing

-- and that way I can translate from a PropertiesJ-set to another value like:

node2valM :: FromJSON a => NodeJ -> Maybe a
node2valM = res2mb . fromJSON . prop2obj . propsn

-- which gives me a Maybe-wrapped value

data NodeJ =
   NJ { idn :: String, labels :: [String], propsn :: PropertiesJ }
      deriving Show

instance FromJSON NodeJ where
   parseJSON obj@(Object o) =
         NJ <$> o .: "id" <*> o .: "labels" <*> parseJSON obj

-- Question: is a Stateful NodeJ-value typed as NodeJS? #justaskin

data RelJ = 
   RJ { idr, kind, startn, endn :: String, propsr :: PropertiesJ }
      deriving Show

instance FromJSON RelJ where
   parseJSON obj@(Object o) =
      RJ <$> o .: "id" <*> o .: "type" <*> o .: "startNode" <*> o .: "endNode"
         <*> parseJSON obj

readGraphJSON :: FilePath -> IO [GraphJ]
readGraphJSON = fmap (map graph . rows . fromJust . decode) . simpleHttp

{-- 
*Y2016.M08.D15.Solution> readGraphJSON twitterGraphUrl ~> tweets
*Y2016.M08.D15.Solution> head tweets ~>
GJ {nodes = [NJ {idn = "1400", labels = ["Source"], propsn = PJ (...) }], ... }
-- Okay, with that: We have tweets, that's fine, with the ["Tweet"] "label"
-- but there are also have other labeled nodes (the example above has a
-- "Source"-labeled node
-- What are all the labels for the nodes in this JSON sample found in the 
-- Graph data of the TwitterRows?
--}

type Label = String

nodeLabels :: [GraphJ] -> [Label]
nodeLabels = nub . concatMap (concatMap labels . nodes)

{--
*Y2016.M08.D15.Solution> nodeLabels tweets
["Source","Tweet","Hashtag","Link","User"]
--}

-- From the relationships data, what are all the "type"-values of the
-- relationships between the tweets and associated information.

relLabels :: [GraphJ] -> [Label]
relLabels = nub . concatMap (map kind . rels)

{--
*Y2016.M08.D15.Solution> relLabels tweets ~>
["USING","TAGS","CONTAINS","MENTIONS","REPLY_TO","RETWEETS"]
--}
