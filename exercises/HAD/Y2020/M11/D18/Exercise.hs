{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D18.Exercise where

{--
Yesterday we updated countries with their wiki-source and capitals (and their
capitals' wiki-sources and their locations, ... but fine).

Today, let's update continents in our graph store with their wiki-sources.

Same problem. We have some weird things wikidata calls 'continent.'

I mean: WEIRD things, yo.

So, we filter on what we have against what wikidata providies.

What wikidata provides is from this SPARQL query:

SELECT DISTINCT ?continent ?continentLabel 
WHERE 
{
  ?dontCare wdt:P30 ?continent.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}

The results from this query is located in this directory.

Same-same as yesterday. Load the data, load the data again, filter the data
against the data, then upload the data.

Wide-load, heavy-load, lode-runner.

Wait.

Wut.
--}

import Data.Aeson
import Data.Aeson.WikiDatum

import Y2020.M10.D14.Solution    -- for Continent and such-like

import Data.Map (Map)

import Data.Relation
import Graph.Query

continentDir :: FilePath
continentDir = "Y2020/M11/D18/"

continentsJSON :: FilePath
continentsJSON = "continents.json"

data ContinentInfo = ContiInfi { conti :: WikiDatum }
   deriving (Eq, Show)

instance FromJSON ContinentInfo where
   parseJSON = undefined

-- so now we can:

type ContinentInfoMap = Map Name ContinentInfo

parseContinents :: FilePath -> IO ContinentInfoMap
parseContinents = undefined

-- Now, filter out `weird` continents from the above from our given ContinentMap

{--
>>> countriesByContinent (workingDir ++ cbc)
...
>>> let contis = it
--}

validateContinents :: ContinentMap -> ContinentInfoMap -> ContinentInfoMap
validateContinents = undefined

-- Now, upload the info on each continent to our graph-store

instance Node ContinentInfo where
   asNode = undefined

uploadContinents :: Endpoint -> [ContinentInfo] -> IO String
uploadContinents = undefined

-- Voilà! Le problème est solvèd! ... that's French... kinda.
