{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D27.Exercise where

import Y2021.M01.D26.Solution (ByCountry, WineriesByCountry, Country)
import qualified Y2021.M01.D26.Solution as WbC

import Y2021.M01.D22.Solution (Wineries, Winery)
import Y2021.M01.D25.Solution (WikiCountry(WC), Neo4jCountry(Neo))
import qualified Y2021.M01.D25.Solution as WW

import qualified Data.Text as T

import Data.Aeson.WikiDatum (Qname, Name, name)
import Graph.Query
import Graph.JSON.Cypher (Cypher, matchSet)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Map (Map)

{--
Yesterday we collected wineries by countries in the neo4j and in the wikidata
sets. ... We didn't correct the names of the countries in the wikidata set
(as we should have), nor did we match wineries in both data sets that matched
names exactly.

Let's do that today, load thosed matched wineries, update the graph-store
and see what we have remaining.
--}

-- 1. correct the names of the countries in the wiki map

correctCountries :: Map WikiCountry Neo4jCountry -> WineriesByCountry -> WineriesByCountry
correctCountries resolver = undefined

-- 2. Add QNames of countries to the graph-store from the wikidata set

addCountryQidQuery :: Neo4jCountry -> Qname -> Cypher
addCountryQidQuery c qid = matchSet "c" c ("qid", qid)

addCountryQids :: Endpoint -> WineriesByCountry -> IO String
addCountryQids url wikiwineries = undefined

-- We've found the matching wineries in a previous exercise. Tomorrow, let's
-- upload those wineries' qid's and geo-locations to the neo4j graph store,
-- using the graph's indexed value to add these data to each node.

