{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D11.Exercise where

{--
SO!

YESTERDAY!

In the bonus section, we saw two things:

1. There are 87 countries lacking their capital-information.
2. All capitals need to have their lat/longs.

I propose we tackle the latter first, because for the latter, at least we 
have the data at hand.

Let's do this.
--}

import Y2020.M12.D10.Solution (Capital)
import Y2020.M11.D17.Solution (CountryInfoMap)
import qualified Y2020.M11.D17.Solution as Caps  -- for capitals and their lat/longs

import Data.Aeson.WikiDatum (Name, LongLat)
import qualified Data.Aeson.WikiDatum as WD

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)

-- Let's load the lat/longs for the capitals from file, then upload them to
-- our graph-store.

{-- 
First: are there any capitals not associated to countries?

The Cypher query:

MATCH (c:Capital) WHERE NOT (c)--(:Country) RETURN count(c)

returns 0

Good.

Second: download all capitals from the graph database.
--}

capitalsQuery :: Cypher
capitalsQuery = "MATCH (c:Capital) RETURN c"

capitals :: Endpoint -> Cypher -> IO (Set Capital)
capitals = undefined

{--
>>> graphEndpoint 
...
>>> let url = it
>>> capitals url capitalsQuery
fromList [Capital "Abu Dhabi",Capital "Addis Ababa",Capital "Algiers",...]

>>> let caps = it
>>> Set.size caps
93

Third: get the countries-capitals map from file via Caps.readCapitals

>>> Caps.readCapitals (Caps.capitalDir ++ Caps.capitalsJSON)
...
>>> let ccm = it
>>> head $ Map.toList ccm
("A Coru\241a",CI {country = WD {qid = "http://www.wikidata.org/entity/Q436147",
                                 name = "A Coru\241a"},
                   capital = WD {qid = "http://www.wikidata.org/entity/Q6063659", 
                                 name = "A Coru\241a"},
                   latLong = point({ latitude: 43.37126, longitude: -8.4188 })})
>>> Map.size ccm
767

Oof-dah! With 767 countries and capitals, perhaps some of the capitals that are
missing in our graph-store are here? We'll explore that another time.

But we need to filter-down some of these data. Let's do that.
--}

filterCaps :: Set Capital -> CountryInfoMap -> CountryInfoMap
filterCaps caps infos = undefined

{--
>>> let fcapInfo = filterCaps caps ccm
>>> Map.size fcapInfo
286

That's a weird number for me, as there are only 93 capitals from the graph-store.
At any rate, it's a smaller number than 767, so that's good...?

Okay, now we have filtered data, but what we care about the filtered set are
the capital names and locations, let's transform the country capital map to
a capital map.
--}

type CapitalLocationMap = Map Capital LongLat

extractCapitalLocations :: Set Capital -> CountryInfoMap -> CapitalLocationMap
extractCapitalLocations = undefined

-- Now that we have those, use the Data.Aeson.WikiDatum.matchSet-function
-- to associate lat/longs to capitals, and upload to the graph-store using
-- the Graph.Query.getGraphResponse function

uploadLatLongs :: Endpoint -> CapitalLocationMap -> IO String
uploadLatLongs url caplocs = undefined  -- geddit? `caplocs`? GEDDIT? ;)

-- Show your results.
