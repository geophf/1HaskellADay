{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D25.Solution where

import Y2021.M01.D22.Solution      -- for wiki wineries with countries

import Control.Arrow ((&&&), (***))

import Data.Aeson.WikiDatum (Name)
import Data.Relation

import Graph.Query
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

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
   asNode (WC n) = constr "AliasedCountry" [("name", n)]

data Neo4jCountry = Neo Name
   deriving (Eq, Ord, Show)

instance Node Neo4jCountry where
   asNode (Neo n) = constr "Country" [("name", n)]

data ALIAS_OF = ALIAS_OF
   deriving (Eq, Ord, Show)

instance Edge ALIAS_OF where
   asEdge = T.pack . show

type AliasRel = Relation WikiCountry ALIAS_OF Neo4jCountry

{-- 
We have an alias relation declared. Now, take the countries exclusive to the
wiki-set and either alias them to a corresponding neo4j country, or, if it's
actually exclusive, upload that country to the graph-store.

>>> readWineries (wineriesDir ++ wineriesJSON)
...
>>> let wikiwineries = it
>>> let wikicountries = uniqueCountries wikiwineries 
>>> graphEndpoint 
...
>>> let url = it
>>> let headN = head :: [Name] -> Name
>>> Set.fromList . map (headN . RR.row) . RR.justRows 
         <$> getGraphResponse url [countriesQuery]
fromList ["Argentina","Armenia","Australia",...]
>>> let neocountries = it
>>> let (wik, neo) = exclusiveCountries wikicountries neocountries 
--}

aliasTheseCountries :: Endpoint -> Map WikiCountry Neo4jCountry -> IO String
aliasTheseCountries url = cyphIt url . map2rels

map2rels :: Map WikiCountry Neo4jCountry -> [AliasRel]
map2rels = map (uncurry (flip Rel ALIAS_OF)) . Map.toList

-- the cypher falls out from the relation created from the wiki-country to
-- the neo4j country ... so, go figure! :D

{--
Our alias map is:

>>> :set -XOverloadedStrings 
>>> let amap = Map.fromList (zipWith (curry (WC *** Neo)) 
                            ["German Democratic Republic", "United Kingdom", 
                             "United States of America"]
                            ["Germany", "England", "US"])

>>> aliasTheseCountries url amap
"{\"results\":...,\"errors\":[]}"

... also, in the graph-store, I crated an index on the aliased country nodes:

$ CREATE INDEX FOR (a:AliasedCountry) ON (a.name)
--}

-- How many countries were aliased? ... 3, of course.

-- Now, we've got to add the non-aliased countries from wikidata:

nonAliasedCountries :: Map WikiCountry a -> Set WikiCountry -> Set Neo4jCountry
nonAliasedCountries mac = 
   Set.map asNeoCountry . flip Set.difference (Map.keysSet mac)

addTheseCountries :: Endpoint -> Set Neo4jCountry -> IO String
addTheseCountries url =
   getGraphResponse url . map addCountryQuery . Set.toList

-- using this Cypher

addCountryQuery :: Neo4jCountry -> Cypher
addCountryQuery = varNode CREATE "foo"

-- How many countries did you add to the graph database?

{--
>>> let nac = nonAliasedCountries amap (Set.map WC wik)
>>> nac
fromList [Neo "Denmark",Neo "Golan Heights",Neo "Japan",Neo "North Macedonia"]

>>> addTheseCountries url nac
"{\"results\":..., \"errors\":[]}"
--}

-- Now that we've put our alias 'table' into the graph, let's create the
-- alias resolver:

countryAliases :: Endpoint -> IO (Map WikiCountry Neo4jCountry)
countryAliases url = Map.fromList . map (toPair . RR.row) . RR.justRows
                 <$> getGraphResponse url [countryAliasesQuery]

-- to define the above you need you some cypher to get the aliases from the
-- graph store:

countryAliasesQuery :: Cypher
countryAliasesQuery =
   T.concat ["MATCH (alias:AliasedCountry)-[:ALIAS_OF]->(country:Country) ",
             "RETURN alias.name, country.name"]

-- and a conversion from the JSON to a set of Name-pairs

toPair :: [Name] -> (WikiCountry, Neo4jCountry)
toPair = WC . head &&& Neo . last

{--
>>> countryAliases url
fromList [(WC "German Democratic Republic",Neo "Germany"),
          (WC "United Kingdom",Neo "England"),
          (WC "United States of America",Neo "US")]
>>> let mac = it
--}

countryAliasResolver :: Map WikiCountry Neo4jCountry -> WikiCountry -> Neo4jCountry
countryAliasResolver table =
   flip maybe id . asNeoCountry <*> flip Map.lookup table

asNeoCountry :: WikiCountry -> Neo4jCountry
asNeoCountry (WC n) = Neo n

-- the behavior here is: "United States of America" -> "US"
-- and:                  "Denmark"                  -> "Denmark"

-- Run all your wiki countries through the alias resolver. How many countries
-- were altered?

{--
>>> Set.map (countryAliasResolver mac . WC) wikicountries 
fromList [Neo "Argentina",Neo "Australia",Neo "Austria",Neo "Bulgaria",
          Neo "Canada",Neo "Chile",Neo "Denmark",Neo "England",Neo "France",
          Neo "Germany",Neo "Golan Heights",Neo "Greece",Neo "Hungary",
          Neo "India",Neo "Israel",Neo "Italy",Neo "Japan",Neo "Moldova",
          Neo "New Zealand",Neo "North Macedonia",Neo "Portugal",
          Neo "South Africa",Neo "Spain",Neo "Switzerland",Neo "US",
          Neo "Ukraine"]
--}
