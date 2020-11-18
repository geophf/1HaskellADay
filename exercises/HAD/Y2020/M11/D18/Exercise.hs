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
import Data.Set (Set)

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

{-- 
We want to, again, validate what are continents (real ones), but before we do
that, we have a midge of data-cleansing to do. There are some continents we
have that are not matched, exactly, in the wikidata returned, but if we 
rename the mismatches, we're back to good. We have to do that to our
wikidata-set.

First off: which continents are missing?
--}

missingContinents :: ContinentMap -> ContinentInfoMap -> Set Continent
missingContinents = undefined

-- then: fix the wikidata:

fixWikidataContinents :: Set Continent -> ContinentInfoMap -> ContinentInfoMap
fixWikidataContinents missingContinents wikidata = undefined

-- now, with the fixed wikidata set, we validate in the wikidata-set which
-- continents are "real" and which are malarky.

validateContinents :: ContinentMap -> ContinentInfoMap -> ContinentInfoMap
validateContinents mycontis fixedwiki = undefined

-- Now, upload the q-id info on each continent to our graph-store

instance Node ContinentInfo where
   asNode = undefined

uploadContinents :: Endpoint -> [ContinentInfo] -> IO String
uploadContinents = undefined

-- Voilà! Le problème est solvèd! ... that's French... kinda.
