{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D25.Exercise where

import Y2021.M01.D22.Solution      -- for wiki wineries with countries

import Data.Aeson.WikiDatum (Name)
import Data.Relation

import Graph.Query
import Graph.JSON.Cypher

{--
So, yesterday, we saw disparities between countries, with some obvious 
differences in names for the same countries.

And then there were other countries that were just different.

How do we separate the two?

Easy, right? An alias is simply an arrow:

(United States of America)-[ALIAS_OF]->(US)

And, the alias resolution is simply an application of that arrow.

But what about the second part, where there is no alias?

That's simply the identity arrow:

(Denmark)-|
    ^-----|

Let's do it!

Part 1:

Download in the exclusive countries for each data-set, as before, and then
upload the alias map with the arrow direction of

 (wiki country name) -[alias_of]-> (neo4j country name)
--}

data WikiCountry = WC Name
   deriving (Eq, Ord, Show)

instance Node WikiCountry where
   asNode = undefined

data Neo4JCountry = Neo Name
   deriving (Eq, Ord, Show)

instance Node Neo4JCountry where
   asNode = undefined

data ALIAS_OF = ALIAS_OF
   deriving (Eq, Ord, Show)

instance Edge ALIAS_OF where
   asEdge = undefined

type AliasRel = Relation WikiCountry ALIAS_OF Neo4JCountry

{-- 
We have an alias relation declared. Now, take the countries exclusive to the
wiki-set and either alias them to a corresponding neo4j country, or, if it's
actually exclusive, upload that country to the graph-store.
--}
