module Y2018.M01.D05.Exercise where

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
 
import Data.Aeson
import Data.Map

-- below imports available via 1HaskellADay git repository

import Data.Relation
import Graph.Query

import Y2017.M12.D20.Exercise  -- for Block
import Y2017.M12.D27.Exercise  -- for DatedArticle

-- Given a set of articles, give a mapping uuid -> sections

sects :: [DatedArticle Value] -> Map String [String]
sects arts = undefined

-- hint: you can read in the articles with (readSample sample) then translate
-- from block to DatedArticle Value with fromJSON

-- Now it become a parsing exercise. A sample section is:

sect :: String
sect = "sports/high-school/scores"

-- For each section output a set of Relation values. Assume the first section
-- is a root node. Assume all node names are unique, meaning that, for example,
-- if "sports" are mentioned in multiple sections, then it's the same sports
-- root node.

sects2rels :: String -> [Relation a rel b]
sects2rels sect = undefined

-- you declare what the types a, rel, and b are

{-- BONUS -----------------------------------------------------------------

Using whatever charting software you like: neo4j, d3js.org, what-have-you,
represent the relations as a graph.
--}

chartSections :: FilePath -> IO ()
chartSections articles = undefined

-- Hint: you can connect to neo4j graph database with Graph.Query functions

-- How many root nodes do you have? What root has the most subsections?
