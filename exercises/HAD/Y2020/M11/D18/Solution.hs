{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Y2020.M11.D18.Solution where

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

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Aeson.WikiDatum

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Data.Relation
import Graph.Query
import Graph.JSON.Cypher (matchSet)

import Y2020.M10.D14.Solution    -- for Continent and such-like

continentDir :: FilePath
continentDir = "Y2020/M11/D18/"

continentsJSON :: FilePath
continentsJSON = "continents.json"

data ContinentInfo = ContiInfi { conti :: WikiDatum }
   deriving (Eq, Show)

instance FromJSON ContinentInfo where
   parseJSON = withObject "Continent" $ \v -> ContiInfi <$> v *: "continent"

-- so now we can:

type ContinentInfoMap = Map Name ContinentInfo

parseContinents :: FilePath -> IO ContinentInfoMap
parseContinents file = maybe Map.empty fmapify . dcdr <$> BL.readFile file
   where mapify = uncurry Map.insert . (name . conti &&& id)
         fmapify = foldr mapify Map.empty
         dcdr :: ByteString -> Maybe [ContinentInfo]
         dcdr = decode

{--
>>> parseContinents (continentDir ++ continentsJSON)
>>> let wcontis = it
>>> Map.size wcontis
24
--}

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
missingContinents contis = Set.difference (Map.keysSet contis) . Map.keysSet

{--
>>> missingContinents contis wcontis
>>> let missin = missingContinents contis wcontis 
>>> missin
fromList []

Huh. So that means:

>>> Set.intersection (Map.keysSet wcontis) (Map.keysSet contis)
fromList ["Africa","Antarctica","Asia","Europe",
          "North America","Oceania","South America"]
>>> it == Map.keysSet contis
True

Well! I'll be! That makes things easier!
--}

-- then: fix the wikidata:

fixWikidataContinents :: Set Continent -> ContinentInfoMap -> ContinentInfoMap
fixWikidataContinents missingContinents = id  -- lol

{--
>>> let fixed = fixWikidataContinents missin wcontis
>>> fixed == wcontis
True
--}

-- now, with the fixed wikidata set, we validate in the wikidata-set which
-- continents are "real" and which are malarky.

validateContinents :: ContinentMap -> ContinentInfoMap -> ContinentInfoMap
validateContinents = Map.filterWithKey . keyCheck . Map.keysSet
   where keyCheck s k = const (Set.member k s)

{--
>>> let vali = validateContinents contis fixed 
>>> Map.size vali
7
--}

-- Now, upload the q-id info on each continent to our graph-store

instance Node ContinentInfo where
   asNode c = constr "Continent" [("name", name $ conti c)]

-- again: we hide the qids as we add them below:

uploadContinents :: Endpoint -> [ContinentInfo] -> IO String
uploadContinents url =
   getGraphResponse url
           . map (uncurry (matchSet "c") . (id &&& ("qid",) . qid . conti))

geaux :: IO String
geaux = parseContinents (continentDir ++ continentsJSON) >>= \wcontis ->
        countriesByContinent (workingDir ++ cbc)         >>= \contis  ->
        graphEndpoint                                    >>= \url     ->
        let missin = missingContinents contis wcontis
            fixed = fixWikidataContinents missin wcontis
            vali = validateContinents contis fixed
        in  uploadContinents url (Map.elems vali)

{--
The resulting (debug) output cypher is:

>>> geaux DEBUG
MATCH (c:Continent { name: "Africa" }) SET c.qid="http://www.wikidata.org/entity/Q15"
MATCH (c:Continent { name: "Antarctica" }) SET c.qid="http://www.wikidata.org/entity/Q51"
MATCH (c:Continent { name: "Asia" }) SET c.qid="http://www.wikidata.org/entity/Q48"
MATCH (c:Continent { name: "Europe" }) SET c.qid="http://www.wikidata.org/entity/Q46"
MATCH (c:Continent { name: "North America" }) SET c.qid="http://www.wikidata.org/entity/Q49"
MATCH (c:Continent { name: "Oceania" }) SET c.qid="http://www.wikidata.org/entity/Q538"
MATCH (c:Continent { name: "South America" }) SET c.qid="http://www.wikidata.org/entity/Q18"

result from uploading data to the graph-store:

>>> geaux
"...\"data\":[]}],\"errors\":[]}"
--}
  
-- Voilà! Le problème est solvèd! ... that's French... kinda.
