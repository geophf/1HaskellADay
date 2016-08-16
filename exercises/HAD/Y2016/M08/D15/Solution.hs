{-# LANGUAGE OverloadedStrings #-}

module Y2016.M08.D15.Solution where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Network.HTTP.Conduit (simpleHttp)

{--
match (:User { name: "1HaskellADay" })-[:POSTS]->(t:Tweet)-[r]->(n)
return t, r, n limit 100
--}

twitterGraphUrl :: FilePath
twitterGraphUrl = "https://raw.githubusercontent.com/geophf/1HaskellADay/"
               ++ "master/exercises/HAD/Y2016/M08/D15/nodes-rels-100.json"

{--
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

data NodeJ =
   NJ { idn :: String, labels :: [String], propsn :: PropertiesJ }
      deriving Show

instance FromJSON NodeJ where
   parseJSON obj@(Object o) =
         NJ <$> o .: "id" <*> o .: "labels" <*> parseJSON obj

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

-- There we have it! Pushing these types to their own module.
