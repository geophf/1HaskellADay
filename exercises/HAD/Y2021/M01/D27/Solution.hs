{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D27.Solution where

import Y2021.M01.D26.Solution (ByCountry, WineriesByCountry, Country)
import qualified Y2021.M01.D26.Solution as WbC

import Y2021.M01.D22.Solution (Wineries, Winery, country)
import Y2021.M01.D25.Solution (WikiCountry(WC), Neo4jCountry(Neo))
import qualified Y2021.M01.D25.Solution as WW

import qualified Data.Text as T

import Data.Aeson.WikiDatum (Qname, Name, name, qid)
import Graph.Query
import Graph.JSON.Cypher (Cypher, matchSet)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Control.Arrow ((&&&), first, (***))
import Data.List (groupBy)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

{--
Yesterday we collected wineries by countries in the neo4j and in the wikidata
sets. ... We didn't correct the names of the countries in the wikidata set
(as we should have), nor did we match wineries in both data sets that matched
names exactly.

Let's do that today, load thosed matched wineries, update the graph-store
and see what we have remaining.
--}

-- 1. correct the names of the countries in the wiki map

{--
>>> graphEndpoint 
...
>>> let url = it
>>> WW.countryAliases url
fromList [(WC "German Democratic Republic",Neo "Germany"),
          (WC "United Kingdom",Neo "England"),
          (WC "United States of America",Neo "US")]
>>> let cam = it
>>> WbC.loadWikiWineries 
...
>>> let wws = it
--}

correctCountries :: Map WikiCountry Neo4jCountry -> WineriesByCountry -> WineriesByCountry
correctCountries resolver = Map.fromList
                          . map (unNeo . fst . head &&& Set.unions . map snd)
                          . groupOn fst
                          . map (first (WW.countryAliasResolver resolver . WC))
                          . Map.toList

-- groupOn from: https://hackage.haskell.org/package/extra-1.7.9/docs/src/Data.List.Extra.html#groupOn

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y

unNeo :: Neo4jCountry -> Country
unNeo (Neo x) = x

{--
>>> let ccm = correctCountries cam wws
>>> Map.map Set.size ccm
fromList [("Argentina",2),("Australia",18),("Austria",3),("Bulgaria",1),
          ("Canada",4),("Chile",1),("Denmark",1),("England",1),("France",165),
          ("Germany",57),("Golan Heights",1),("Greece",1),("Hungary",4),
          ("India",1),("Israel",9),("Italy",9),("Japan",6),("Moldova",2),
          ("New Zealand",4),("North Macedonia",2),("Portugal",10),
          ("South Africa",12),("Spain",32),("Switzerland",10),("US",247),
          ("Ukraine",2)]

You see that Germany is merged with the German Democratic Republic and that
US and UK (England) are renamed.
--}

-- 2. Add QNames of countries to the graph-store from the wikidata set

addCountryQidQuery :: Neo4jCountry -> Qname -> Cypher
addCountryQidQuery c qid = matchSet "c" c ("qid", qid)

addCountryQids :: Endpoint -> WineriesByCountry -> IO String
addCountryQids url = getGraphResponse url
            . map (uncurry addCountryQidQuery . (Neo *** qid . country . head . Set.toList))
            . Map.toList

{--
>>> addQids url ccm
"{\"results\":...,\"errors\":[]}"
--}

-- We've found the matching wineries in a previous exercise. Tomorrow, let's
-- upload those wineries' qid's and geo-locations to the neo4j graph store,
-- using the graph's indexed value to add these data to each node.
