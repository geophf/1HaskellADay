{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D26.Exercise where

{--
At this point in the game, we have two largish wine data sets, one from 
wikidata, one from neo4j, both with their distinctive 'personalities' (?) shall
we say? Let's grasp at understanding each data set and see where we can find
similarities and where we can mend differences.

We started this work with country aliases up to now, let's add in wineries.

From each data set, return a wineries-by-country mapping.
--}

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Text as T

import Data.Aeson.WikiDatum (Name)
import Graph.Query
import Graph.JSON.Cypher (Cypher)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D22.Solution (Wineries, Winery)
import qualified Y2021.M01.D22.Solution as WW  -- WikiWineries

type WineriesByCountry = Map Country (Set Winery)
type Country = Name

wikiWineriesByCountry :: Wineries -> WineriesByCountry
wikiWineriesByCountry = undefined

-- getting wineries by country from the neo4j graph involves query-magic

countryWineriesQuery :: Cypher
countryWineriesQuery =
   T.concat ["MATCH (w:Winery)-[:FROM_PROVENCE]->()-[:PROVINCE_COUNTRY]->",
             "(c:Country) RETURN c.name, w.name"]

neo4jWineriesByCountry :: Endpoint -> IO WineriesByCountry
neo4jWineriesByCountry = undefined

-- How many wineries-by-country are there in each data set? Particularly,
-- how many wineries are in "No Country [... for old men, lol]"?

-- we'll start looking at matching wineries across data sets, ... tomorrow.

-- also: do we want to enhance Countries and Wineries with QNames from
-- wikidata? Hm.
