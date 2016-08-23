module Y2016.M08.D23.Exercise where

import Data.Aeson

import Data.Relation
import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Graphs

import Y2016.M08.D15.Exercise (twitterGraphUrl)

{--
We've been concentrating on the last little while on the internal structure
of the nodes: what are the data of a tweet, or a user, or a hashtag.

That's all well and good, but there's something to be said about the structure
of the lattice formed by the interactions of these nodes. In fact, Category
Theory (for example) says that what's inside the nodes (units) is unimportant,
what is important is the structures they form.

So, let's look at and visualize these structures. From the previous set of
exercises we know what the types of the nodes are (or you can look at the
JSON yourself at twitterGraphUrl) and we also know the types of the relations.

Create a data type that has these node-types, read in the node types and
populate a set of relations from the above-imported twitterGraphUrl JSON.
--}

data TwitterNode = WhatAreTheNodeTypeNames

instance Node TwitterNode where
   asNode = undefined

-- do the same for the kinds of relations between twitter nodes

data TwitterRelation = WhatAreTheKindsOfRelations
   deriving Show

instance Edge TwitterRelation where
   asEdge = undefined

-- with the above defined, read in the twitter graph-JSON as a set of relations

type Twitter3 = Relation TwitterNode TwitterRelation TwitterNode

readTwitterGraph :: FilePath -> IO [Twitter3]
readTwitterGraph = undefined

-- How many relations did you realize from the twitter graph JSON?

{-- BONUS -----------------------------------------------------------------

Great. Now upload the above relations as a cypher query to a graph database.
View and share your results.
--}

twitterAsGraph :: Endpoint -> [Twitter3] -> IO String
twitterAsGraph = undefined

-- what is the longest relation? What (sub)graph has the most connected nodes?
