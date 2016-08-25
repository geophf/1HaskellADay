module Y2016.M08.D23.Solution where

import Control.Arrow ((&&&), (>>>))
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import System.Environment (getEnv)

import Data.Relation
import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Graphs (GraphJ, NodeJ, RelJ)
import qualified Graph.JSON.Cypher.Read.Graphs as G

import Y2016.M08.D15.Exercise (twitterGraphUrl)

data TNK = Link | Source | Tweet | Hashtag | User
   deriving (Eq, Ord, Enum, Show, Read)

data TwitterNode = TN { kind :: TNK, idn :: String }
   deriving (Eq, Ord, Show)

-- for this exercise we just want to see structure, not (internal) data
-- but to distinguish nodes, they must be disjoined by, e.g.: ids

instance Node TwitterNode where
   asNode (TN k idx) = show k ++ " { kind: '" ++ show k ++ "', idx: '" ++ idx ++ "' }"

-- do the same for the kinds of relations between twitter nodes

data TRK = USING | CONTAINS | TAGS | POSTS | FOLLOWS | MENTIONS | RETWEETS | REPLY_TO
   deriving (Eq, Ord, Enum, Show, Read)

data TwitterRel = TR { typ :: TRK } -- the relations are distinguished by node
   deriving (Eq, Ord, Show)

instance Edge TwitterRel where
   asEdge = show . typ

-- with the above defined, read in the twitter graph-JSON as a set of relations

type Twitter3 = Relation TwitterNode TwitterRel TwitterNode

-- to parse the nodes and relations we need, surprisingly, the nodes and the
-- relations of the graph

{--
Here is the structure of a graph:

"graph":
 {"nodes":[{"id":"1400","labels":["Source"],
            "properties":{"name":"Twitter Web Client"}},
           {"id":"255","labels":["Tweet"],
            "properties":{"id":727179491756396500,
                          "id_str":"727179491756396546",
                          "text":"April 2016 @1HaskellADay #haskell problems and solutions posted at https://t.co/QPP9j2PLsX",
                          "created_at":"Mon May 02 16:54:35 +0000 2016",
                          "favorites":0}}],
  "relationships":[{"id":"3799","type":"USING","startNode":"255",
                    "endNode":"1400","properties":{}}]}}

So, here's the plan: we parse the nodes, index them by id, then we reify the
relation from the node-map.
--}

type NodeMap = Map String TwitterNode

parseNodes :: GraphJ -> NodeMap
parseNodes = Map.fromList . map (G.idn &&& node2tn) . G.nodes

node2tn :: NodeJ -> TwitterNode
node2tn = read . head . G.labels &&& G.idn >>> uncurry TN

parseRels :: NodeMap -> GraphJ -> [Twitter3]
parseRels nodes = mapMaybe (parseRel nodes) . G.rels

parseRel :: NodeMap -> RelJ -> Maybe Twitter3
parseRel nodes (G.RJ _ kind st end _) =
   flip Rel (TR $ read kind) <$> lk st <*> lk end
      where lk = flip Map.lookup nodes

-- so with the above definitions we define the below:

readTwitterGraph :: FilePath -> IO [Twitter3]
readTwitterGraph =
   fmap (concatMap (parseNodes &&& id >>> uncurry parseRels)) . G.readGraphJSON

-- How many relations did you realize from the twitter graph JSON?

-- *Y2016.M08.D23.Solution> readTwitterGraph twitterGraphUrl ~> relz ~> length ~> 100

{-- BONUS -----------------------------------------------------------------

Great. Now upload the above relations as a cypher query to a graph database.
View and share your results.
--}

twitterAsGraph :: Endpoint -> [Twitter3] -> IO String
twitterAsGraph url = getGraphResponse url . map (mkCypher "a" "rel" "b")

-- what is the longest relation? What (sub)graph has the most connected nodes?

{--
*Y2016.M08.D23.Solution> getEnv "CYPHERDB_ACCESS" ~> url
*Y2016.M08.D23.Solution> readTwitterGraph twitterGraphUrl >>= twitterAsGraph url
(graph is viewable on solution-tweet)

Inside neo4j browser:

$ match (n) return count(n) ~> 66 nodes

$ match (n)--(m) return n,count(m) order by count(m) desc ~>
node Source, idx 1400, has 24 descendent/children nodes

$ MATCH p=(n)-[*]->(m)
WITH COLLECT(p) AS paths, MAX(length(p)) AS maxLength 
RETURN paths, maxLength order by maxLength desc ~> 2

So, although we have a lot of nodes and relations here ('a lot' being a (very)
relative term in the big-data sense), the longest path is just 2 for this
data set.
--}
