module Y2018.M01.D05.Solution where

{--
Let's take a break from databases and Haskell and look at something completely
different:

data schemes and Haskell!

AHA!

So, today's Haskell problem is this.

We have a semi-structure in the DatedArticle-type called 'sections,' currently
typed as [String], but what is the structure hidden in those String values?

We will find that out today.
--}
 
import Control.Arrow ((&&&))

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

-- below imports available via 1HaskellADay git repository

import Control.Scan.CSV
import Data.Relation
import Graph.Query

import Y2017.M12.D20.Solution  -- for Block
import Y2017.M12.D27.Solution  -- for DatedArticle

-- Given a set of articles, give a mapping uuid -> sections

sects :: [DatedArticle a] -> Map String [String]
sects = foldr (uncurry Map.insert . sects') Map.empty

sects' :: DatedArticle a -> (String, [String])
sects' = uuid &&& sections

-- hint: you can read in the articles with (readSample sample) then translate
-- from block to DatedArticle Value with fromJSON

-- Now it become a parsing exercise. A sample section is:

sect :: String
sect = "sports/high-school/scores"

-- For each section output a set of Relation values. Assume the first section
-- is a root node. Assume all node names are unique, meaning that, for example,
-- if "sports" are mentioned in multiple sections, then it's the same sports
-- root node.

data Section = SECTION { name :: String, depth :: Int } | ROOT
   deriving (Eq, Show)

instance Node Section where
   asNode (SECTION s d) =
     "SECTION { name: '" ++ s ++ "', depth: " ++ show d ++ " }"
   asNode ROOT = "ROOT"

data Rel = SUBSUMES deriving (Eq, Show)

instance Edge Rel where asEdge = show

sects2rels :: String -> [Relation Section Rel Section]
sects2rels = sects2 1 . rendBy (== '/')

sects2 :: Int -> [String] -> [Relation Section Rel Section]
sects2 depth (h:t) =
   let node = SECTION h depth in
   Rel ROOT SUBSUMES node:sects3 (succ depth) node t

sects3 :: Int -> Section -> [String] -> [Relation Section Rel Section]
sects3 depth s (h:t) =
   let nd = SECTION h depth in Rel s SUBSUMES nd:sects3 (succ depth) nd t
sects3 _     _ []    = []

{--
>> sects2rels sect
[Rel ROOT SUBSUMES (SECTION {name = "sports", depth = 1}),Rel (SECTION {name = "sports", depth = 1}) SUBSUMES (SECTION {name = "high-school", depth = 2}),Rel (SECTION {name = "high-school", depth = 2}) SUBSUMES (SECTION {name = "scores", depth = 3})]
--}

{-- BONUS -----------------------------------------------------------------

Using whatever charting software you like: neo4j, d3js.org, what-have-you,
represent the relations as a graph.
--}

blk2art :: Block -> Maybe (DatedArticle Value)
blk2art = b2a . fromJSON

b2a :: Result (DatedArticle Value) -> Maybe (DatedArticle Value)
b2a (Error _) = Nothing
b2a (Success a) = Just a

chartSections :: FilePath -> IO String
chartSections articles =
   sects . mapMaybe blk2art . rows <$> readSample articles >>= \yo ->
   let rels = concatMap sects2rels (concat $ Map.elems yo) in
   graphEndpoint >>= flip cyphIt rels

-- Hint: you can connect to neo4j graph database with Graph.Query functions

-- How many root nodes do you have? What root has the most subsections?

{--
>>> chartSections "Y2017/M12/D20/sample.json" 
... ,{\"columns\":[],\"data\":[]}],\"errors\":[]}"

There are 32 nodes, 9 of those nodes are root (see graph in neo4j browser)

$ match (:ROOT)-[:SUBSUMES]->(n:SECTION) return n.name

n.name
"vagrowler"
"vow-bride"
"business"
"entertainment"
"inside-business"
"life"
"opinion"
"news"
"sports"
--}
