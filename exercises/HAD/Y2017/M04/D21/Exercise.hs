module Y2017.M04.D21.Exercise where

-- below imports available via 1HaskellADay git repository

import Control.Scan.CSV
import Wikidata.Query.Aeson
import Wikidata.Query.Builder
import Wikidata.Query.Endpoint

{--
Question:

1. what are the cities, populations, and locations of Alaska?

To answer that we have:
--}

wikipedia :: FilePath
wikipedia = "https://en.wikipedia.org/wiki/List_of_cities_in_Alaska"

{--
How do we scan that in and use that? How do we plot that?

Hm. Another question: why do we want to reinvent the wheel?

-- or --

Wikidata can plot information on a map for you "fo' free!"

Compose a SPARQL wikidata query that asks for the above information:
--}

alaskanCitiesSPARQL :: SPARQL
alaskanCitiesSPARQL = undefined

-- oh, and you can look up how to do that and what are the qnames. I did.

-- Now get the results back as set of Haskell values:

data AlaskanCity = NorthToAlaska { city, pop, location :: String }
   deriving (Eq, Ord, Show)

alaskanCities :: SPARQL -> IO [AlaskanCity]
alaskanCities = undefined

-- How many Alaskan cities are returned from the wikidata query?
-- How many Alaskan cities are listed at the wikipedia-url?
-- How would you fix this discrepancy? Or, what other source would you use?

-- BONUS -----------------------------------------------------------------
-- You see all AlaskanCity values are of type String. A glaring problem with
-- parseVal that needs to be fixed. How would you fix that?
