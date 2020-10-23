module Y2020.M10.D12.Exercise where

import Data.Map (Map)
import Data.Text

import Data.Aeson

-- Okay, we have a query that we want to make to wikidata. Let's make it.

-- import Wikidata.Query.Builder

-- Okay, that doesn't work with newer versions of GHCI. Oh, well.

{--
The SPARQL query:

SELECT ?item ?itemLabel ?icao ?countryLabel ?loc 
WHERE {  ?item wdt:P31 wd:Q695850.  
       ?item wdt:P239 ?icao. 
       ?item wdt:P17 ?country.
       ?item wdt:P625 ?loc
       SERVICE wikibase:label {
             bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en".
       } 
      }

yields airbases.json (at this directory).
--}

workingDir :: FilePath
workingDir = "Y2020/M10/D12/"

file :: FilePath
file = "airbases.json"

-- As you see above, the data model is something like this:

type Key = String
type Entity = Text
type Icao = String
type Country = String

data LongLat = Point Double Double
   deriving (Eq, Ord, Show)

data AirBase = Base Key Entity Icao Country LongLat
   deriving (Eq, Ord, Show)

-- 1. Load in the airbases.

instance FromJSON LongLat where
   parseJSON = undefined

instance FromJSON AirBase where
   parseJSON = undefined

loadBases :: FilePath -> IO (Maybe [AirBase])
loadBases = undefined

-- 2. How many airbases are there?

-- 3. Actually, that's a tricky question, because, as you examine the data, 
--    you see there are duplicate results. And a `distict` in the SPARQL
--    query doesn't eliminate the duplicates in the result.

-- Let's solve that problem.

byICAO :: [AirBase] -> Map Icao AirBase
byICAO = undefined

-- 4. Now: how many bases are there, without duplicates?
