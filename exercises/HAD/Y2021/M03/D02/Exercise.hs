{-# LANGUAGE OverloadedStrings #-}

module Y2021.M03.D02.Exercise where

import Y2021.M03.D01.Solution

{--
Yesterday we approved a set of aliased wineries, ending up with a

Map Idx Winery

set of approved aliases.

Today, we're going to store the results in the graph.

But what does that mean?

1. We need to update the graph-winery node with QNames and the location, and
2. we need to alias the wiki-winery name to the graph-winery node.

Let's do these things.
--}

import Data.Aeson

import Data.Map (Map)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Graph.Query
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Aeson.WikiDatum

import Y2021.M01.D21.Solution (Idx)
import Y2021.M01.D22.Solution                   -- for wineries
import Y2021.M01.D25.Solution hiding (toPair)   -- country-alias resolver
import Y2021.M02.D22.Solution (normalizeWikiCountry)

-- so, step 1: we have (idx, winery) to upload to the graph store

-- question 1: are there contries in the matched set that have QNames that
-- in the graph-store do not?

-- question 1.a.: what are the countries of the matched set?
--            ... Wait. I meant to say 'aliased countries.'

matchedCountries :: Map WikiCountry Neo4jCountry -> AliasedWineries
                                                 -> Set WikiDatum
matchedCountries = undefined

-- question 1.b.: Are any of those countries in the graph-store missing
-- QNames?

countriesQuery :: Set WikiDatum -> Cypher
countriesQuery wikiCountries =
   T.pack (unlines ["MATCH (c:Country) WHERE c.name IN ",
                    show (map name $ Set.toList wikiCountries),
                    "RETURN c.name, c.qid"])

graphCountries :: Endpoint -> Set WikiDatum -> IO (Map Name Qname)
graphCountries url wikiCountries = undefined

-- of course, we need to translate the rows returned to Map-entries

toPair :: [Value] -> Maybe (Name, Qname)
toPair [n, q] = undefined

{--
>>> graphEndpoint >>= flip graphCountries countrs
fromList [("France","http://www.wikidata.org/entity/Q142"),
          ("Israel","http://www.wikidata.org/entity/Q801"),
          ("South Africa","http://www.wikidata.org/entity/Q258"),
          ("Spain","http://www.wikidata.org/entity/Q29")]

... Okay, they all have QNames. Good. No need, then, to update any countries.

Now, we add the wiki-attributes of the wineries in the graph-store.
--}

updateGraphWineryPropertiesQuery :: (Idx, Winery) -> Cypher
updateGraphWineryPropertiesQuery (idx, Winery (WD qn _) _ loc) =
   T.pack (unwords ["MATCH (w:Winery) WHERE id(w) = ", show idx,
                    "SET w += { qid: ", show qn, ", location: ",
                    show loc, " }"])         

-- we also need to map the alias to the graph-winery

aliasToWineryQuery :: (Idx, Winery) -> Cypher
aliasToWineryQuery (idx, Winery n _ _) = 
   T.pack (unwords ["CREATE (a:AliasedWinery { name:", show (name n), "})",
                    "WITH a AS alias",
                    "MATCH (w:Winery) WHERE id(w) =", show idx,
                    "CREATE (alias)-[:ALIAS_OF]->(w)"])

-- update the graph-winery nodes and alias them.
