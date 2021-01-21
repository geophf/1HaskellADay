{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D20.Exercise where

import Data.Aeson
import Data.Aeson.WikiDatum

import Graph.Query
import Graph.JSON.Cypher

import Data.Map (Map)

{--
We have a JSON file from wikidata with wineries and their geo-locations, from
this SPARQL query:

SELECT ?item ?itemLabel ?location
WHERE 
{
  ?item wdt:P31 wd:Q156362.
  ?item wdt:P625 ?location.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}

Parse the JSON.
--}

data Winery = Winery { name :: WikiDatum, location :: LongLat }
   deriving (Eq, Ord, Show)

instance FromJSON Winery where
   parseJSON = undefined

type Wineries = Map Name Winery

wineriesJSON, wineriesDir :: FilePath
wineriesJSON = "winery-locations.json"
wineriesDir = "Y2021/M01/D20/"

readWineries :: FilePath -> IO Wineries
readWineries = undefined

-- How many wineries are there?
