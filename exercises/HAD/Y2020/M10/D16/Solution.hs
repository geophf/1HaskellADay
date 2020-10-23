{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D16.Solution where

{--
`sequence` is really cool: a neat trick.

The problem is that we have to use it in the first place. The question becomes
if the data structure you are using doesn't give you what you need, then you
need to move to a better data model. Data.Map isn't bidirecional, and that's
fine in many cases, but isn't in this case where we needed to key from the
country to the continent, and not the waythe original structure was built.

So. What data structure?

Hello, Graph.
--}

import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows (justRows, QueryResult)

import Data.Relation

import qualified Data.Map as Map
import qualified Data.Text as T

import Y2020.M10.D12.Solution   -- for Country-type
import Y2020.M10.D14.Solution
import Y2020.M10.D15.Solution

{--
Take the countries and continents from before and structure these data in a
graph.

In which continent is Andorra?
What are the countries in ... 'Oceania'?
What are all the continents? What is each continent's countries?
--}

-- Geo is actually a dependently-typed family of types, but oh, well.

data Geo = Country Country | Continent Continent
   deriving (Eq, Show)

instance Node Geo where
   asNode (Country coun) = T.concat ["Country { name: \"", coun, "\" }"]
   asNode (Continent cont) = T.concat ["Continent { name: '", cont, "' }"]

data IN = IN
   deriving Show

instance Edge IN where
   asEdge = const "IN"

type Cy = Relation Geo IN Geo

-- type Cy should be stricter: A continent cannot be in a country, but oh, well.

type Graph = Endpoint    -- ... *cough*

relationalize :: CountryMap -> [Cy]
relationalize = map relit . Map.toList

relit :: (Country, Continent) -> Cy
relit (country, continent) = Rel (Country country) IN (Continent continent)

{--
>>> countriesByContinent (Y2020.M10.D14.Solution.workingDir ++ cbc)
>>> let m = it
>>> let cm = countryMap m
>>> graphEndpoint
>>> let url = it
>>> take 2 $ relationalize cm
[Rel (Country "Afghanistan") IN (Continent "Asia"),
 Rel (Country "Albania (Shqip\235ria)") IN (Continent "Europe")]

>>> cyphIt url (relationalize cm) 
--}

continentOf :: Graph -> Country -> IO QueryResult
continentOf url countr = getGraphResponse url (query (Country countr))

{--
>>> justRows <$> continentOf url "Andorra"
[TR {row = Array [Object (fromList [("name",String "Europe")])]}]
--}

query :: Geo -> [Cypher]
query a = [T.concat ["MATCH (", asNode a, ")-[:IN]-(b) RETURN b"]]

countriesOf :: Graph -> Continent -> IO QueryResult
countriesOf url conti = getGraphResponse url (query (Continent conti))

{--
>>> take 3 . justRows <$> countriesOf url "Oceania"
[TR {row = Array [Object (fromList [("name",String "Nauru")])]},
 TR {row = Array [Object (fromList [("name",String "Tonga")])]},
 TR {row = Array [Object (fromList [("name",String "Tuvalu")])]}]
--}
