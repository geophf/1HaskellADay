{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D22.Solution where

import Control.Arrow ((&&&))

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Foldable (toList)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Graph.Query
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Aeson.WikiDatum

{--
Okay, same, but different, because now we pulled countries from wikidata into 
the mix with the following SPARQL query:

SELECT ?item ?itemLabel ?country ?countryLabel ?location
WHERE
{
  ?item wdt:P31 wd:Q156362.
  ?item wdt:P625 ?location.
  ?item wdt:P17 ?country.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}

The new wineries with their countries are at:
--}

wineriesDir, wineriesJSON :: FilePath
wineriesDir = "Y2021/M01/D22/"
wineriesJSON = "wineries-countries.json"

-- same drill: load in the JSON file

data Winery = Winery { winery :: WikiDatum, country :: WikiDatum,
                       location :: LongLat }
   deriving (Eq, Ord, Show)

instance FromJSON Winery where
   parseJSON = withObject "Winery" $ \v ->
      Winery <$> v *: "item" <*> v *: "country" <*> v @: "location"

type Wineries = Map Name Winery

readWineries :: FilePath -> IO Wineries
readWineries json =
   Map.fromList . map (name . winery &&& id) . fromMaybe [] . decode
               <$> BL.readFile json

{--
>>> readWineries (wineriesDir ++ wineriesJSON)
fromList [("21 Cellars",
           Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q4630984",
                                name = "21 Cellars"},
                   country = WD {qid = "http://www.wikidata.org/entity/Q30",
                                 name = "United States of America"}, 
                   location = point({ latitude: 47.2675, longitude: -122.471 })}),...]

>>> let wikiwineries = it
--}
-- cool! How many unique countries are in the wiki data set?

class HasCountry a where
   state :: a -> Name

instance HasCountry Winery where
   state = name . country

type Countries = Set Name

uniqueCountries :: HasCountry a => Foldable t => t a -> Countries
uniqueCountries = Set.fromList . map state . toList

{--
>>> let wikicountries = uniqueCountries wikiwineries 
>>> Set.size wikicountries 
27
>>> wikicountries 
fromList ["Argentina","Australia","Austria","Bulgaria","Canada","Chile",
          "Denmark","France","German Democratic Republic","Germany",
          "Golan Heights","Greece","Hungary","India","Israel","Italy","Japan",
          "Moldova","New Zealand","North Macedonia","Portugal","South Africa",
          "Spain","Switzerland","Ukraine","United Kingdom",
          "United States of America"]
--}

-- Okay, for the graph database: how many unique countries does it have?

countriesQuery :: Cypher
countriesQuery = "MATCH (n:Country) RETURN DISTINCT n.name"

-- do the magic to extract a set of countries from the graph

{--
>>> graphEndpoint 
...
>>> let url = it
>>> let headN = head :: [Name] -> Name
>>> Set.fromList . map (headN . RR.row) . RR.justRows
               <$> getGraphResponse url [countriesQuery]
fromList ["Argentina","Armenia","Australia","Austria","Bosnia and Herzegovina",...]
>>> let neo4jCountries = it
>>> Set.size neo4jCountries 
44
--}

-- Now: how many countries intersect?

sharedCountries :: Countries -> Countries -> Countries
sharedCountries = Set.intersection

{--
>>> let shar = sharedCountries wikicountries neo4jCountries 
>>> Set.size shar
20
>>> shar
fromList ["Argentina","Australia","Austria","Bulgaria","Canada","Chile",
          "France","Germany","Greece","Hungary","India","Israel","Italy",
          "Moldova","New Zealand","Portugal","South Africa","Spain",
          "Switzerland","Ukraine"]
--}

-- How many countries are unique to the wiki-set? How many countries are
-- unique to the graph-set?

exclusiveCountries :: Countries -> Countries -> (Countries, Countries)
exclusiveCountries wik neo =
   let inter = sharedCountries wik neo
       diff  = flip Set.difference inter
   in  (diff wik, diff neo)

{--
>>> let (wik, neo) = exclusiveCountries wikicountries neo4jCountries 
>>> Set.size wik
7
>>> wik
fromList ["Denmark","German Democratic Republic","Golan Heights","Japan",
          "North Macedonia","United Kingdom","United States of America"]
>>> Set.size neo
24
>>> neo
fromList ["Armenia","Bosnia and Herzegovina","Brazil","China","Croatia",
          "Cyprus","Czech Republic","Egypt","England","Georgia","Lebanon",
          "Luxembourg","Macedonia","Mexico","Morocco","No Country","Peru",
          "Romania","Serbia","Slovakia","Slovenia","Turkey","US","Uruguay"]

LOL! "No Country"? Really???
--}

-- HOW DO WE RECONCILE THESE DIFFERENCES? WHY CAN WE ALL JUST GET ALONG???

-- Rhetorical question? Maybe. But we do want data types to be the same for
-- when we merge data. That's the problem we saw yesterday when wineries'
-- names differ by just a bit.

-- I think tomorrow we'll look at resolving aliased entities.
