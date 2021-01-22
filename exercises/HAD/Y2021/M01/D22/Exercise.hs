{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D22.Exercise where

import Data.Aeson

import Data.Map (Map)
import Data.Set (Set)

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
   parseJSON = undefined

type Wineries = Map Name Winery

readWineries :: FilePath -> IO Wineries
readWineries = undefined

-- cool! How many unique countries are in the wiki data set?

class HasCountry a where
   state :: a -> Name

instance HasCountry Winery where
   state = undefined

type Countries = Set Name

uniqueCountries :: HasCountry a => Foldable t => t a -> Countries
uniqueCountries = undefined

-- Okay, for the graph database: how many unique countries does it have?

countriesQuery :: Cypher
countriesQuery = "MATCH (n:Country) RETURN DISTINCT n.name"

-- do the magic to extract a set of countries from the graph

-- Now: how many countries intersect?

sharedCountries :: Countries -> Countries -> Countries
sharedCountries = undefined

-- How many countries are unique to the wiki-set? How many countries are
-- unique to the graph-set?

exclusiveCountries :: Countries -> Countries -> (Countries, Countries)
exclusiveCountries = undefined

-- HOW DO WE RECONCILE THESE DIFFERENCES? WHY CAN WE ALL JUST GET ALONG???

-- Rhetorical question? Maybe. But we do want data types to be the same for
-- when we merge data. That's the problem we saw yesterday when wineries'
-- names differ by just a bit.
