{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D18.Exercise where

{-- if you look at how we upload capitals at ...

import Y2020.M12.D10.Solution (Capital, Capital(Capital))
import qualified Y2020.M12.D11.Solution as Caps

... you see the work of uploading Capital lat/long data to the graph-store
is accomplished, but today, we want to dowload the capitals with the
lat/longs, given the capital's country. Let's do that.

... I think we may need to create a new Capital-type, including the lat/long,
to download it from the graph-store ... OR we use the CountryInfo-type, so
we can populate a map with the Keyhole Markup Language/KML.

An example of us populating a map with KML is at 

Y2020.M11.D23.Solution.nato

... I think this example needs to be used as Data.XHTML.KML-documentation.

But, one way to look at today's #haskell exercise is a rewrite of the
nato-function to work with data from the graph store ... and to display
any alliance, just not NATO. Today, we want to display the Five Power
Defence Arrangements, and we can do that with

Y2020.M11.D23.Solution.kmlifyAlliance

we just need to populate the CountryInfoMap and the AllianceMap to do so.

We have our work cut out for us.
--}

import Data.Aeson
import Data.Aeson.Types (Parser)

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows (TableRow)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Aeson.WikiDatum
import Data.XHTML.KML

import Y2020.M10.D30.Solution hiding (name)     -- for Alliance
import Y2020.M11.D17.Solution                   -- for CountryInfo and ..-Map
import Y2020.M11.D23.Solution                   -- KMLificiation
import Y2020.M12.D15.Solution                   -- for capital

-- So, where we left off yesterday:

import Y2020.M12.D16.Solution

-- ... was to import the countries of a given alliance. Let's add capitals.
-- To do this, we need to ingest the capitals from the JSON returned

instance FromJSON Capital where
   parseJSON = undefined

{--
A sample capital returned from the graph-store is:

>>> getGraphResponse url [capitalOfQuery (Country "Malaysia" Nothing [])]
{"results":[{"columns":["c"],
             "data":[{"row":[{"name":"Kuala Lumpur",
                              "location":{"type":"Point",
                                  "coordinates":[101.695277777,3.147777777],
                                  "crs":{"srid":4326,"name":"wgs-84",
                                         "type":"link",
                                         "properties":{"href":"http://spatialreference.org/ref/epsg/4326/ogcwkt/",               
                                                 "type":"ogcwkt"}}},
                              "qid":"http://www.wikidata.org/entity/Q1865"}],
 "meta":[{"id":1292,"type":"node","deleted":false}]}]}],"errors":[]}

Sheesh.
--}

convertPoint :: Value -> Parser LongLat
convertPoint = undefined

-- We also need a query to extract the capital for a given country:

capitalOfQuery :: Country -> Cypher
capitalOfQuery (Country c _ _) =
   T.concat ["MATCH (:Country { name: \"", c, "\" })-[:CAPITAL]->(c:Capital) ",
             "RETURN c"]

-- so, with the capital parser defined above, we get the following

sampleCapital :: String
sampleCapital =
   concat ["{\"name\":\"Kuala Lumpur\",",
             "\"location\":{\"type\":\"Point\",",
                            "\"coordinates\":[101.695277777,3.147777777],",
                            "\"crs\":{\"srid\":4326,\"name\":\"wgs-84\",",
                                      "\"type\":\"link\",",
                                      "\"properties\":{\"href\":\"http://spatialreference.org/ref/epsg/4326/ogcwkt/\",",
                                                       "\"type\":\"ogcwkt\"}}},",
             "\"qid\":\"http://www.wikidata.org/entity/Q1865\"}"]

{--
>>> (decode $ BL.pack sampleCapital) :: Maybe Capital
Just (Capital (WD {qid = "http://www.wikidata.org/entity/Q1865",
                   name = "Kuala Lumpur"})
               point({ latitude: 3.147777777, longitude: 101.695277777 }))
--}

fetchCapital :: Endpoint -> Country -> IO (Maybe Capital)
fetchCapital url country = undefined

{--
First things first. Let's get our alliance and its countries:

>>> :set -XOverloadedStrings

>>> graphEndpoint
...
>>> let url = it
>>> getGraphResponse url [allianceCountriesQuery fpda]
"{\"results\":[{\"columns\":[\"c\"],\"data\":...
>>> let resp = it
>>> (RR.justRows resp) :: [TableRow [Country]]
[TR {row = [Country {country = "United Kingdom", ...}]}]

So we have a list of countries, but, for an Alliance, we need the Set of
countries to create our Alliance

fetchAlliance :: Endpoint -> Name -> IO (Maybe Alliance)
fetchAlliance url allianceName = undefined

I have `fetchAlliance` commented out, because we can do better. See below.
--}

-- The below two functions may be useful to getting to the above fetch-function.

fetchAllianceNames :: Endpoint -> Name -> IO [AllianceNames]
fetchAllianceNames url allianceName = undefined

{--
>>> fetchAllianceNames url fpda
[AN "Five Power Defence Arrangements" []]
--}

fetchAllianceCountries :: Endpoint -> Name -> IO (Set Country)
fetchAllianceCountries url allianceName = undefined

{--
>>> fetchAllianceCountries url fpda
fromList [Country {country = "Australia", ... }]

>>> let ctrs = it
--}

-- work to getting to the above 2 fetchers are in Y2020.M12.D16.Solution

{--
>>> fetchAlliance url fpda
Just (Alliance {name = "Five Power Defence Arrangements", aliases = fromList [],
                countries = fromList ["Australia","Malaysia","New Zealand",
                                      "Singapore","United Kingdom"]})

Okay. Great. Now, let's get the capitals of the countries... (the declaration
is all the way at the top of this module).
--}

-- with the country and the capital you can build a CountryInfo value, and
-- from there, a CountryInfoMap

countryInfoFor :: Country -> Maybe Capital -> Maybe CountryInfo
countryInfoFor = undefined

-- from thence:

fetchCountryInfoMap :: Endpoint -> Set Country -> IO CountryInfoMap
fetchCountryInfoMap = undefined

{--
>>> fetchCountryInfoMap url ctrs
{("Australia",CI {country = WD {qid = "http://www.wikidata.org/entity/Q408",
                                name = "Australia"},
                  capital = WD {qid = "http://www.wikidata.org/entity/Q3114",
                                name = "Canberra"},
                  latLong = point({ latitude: -35.293055555, longitude: 149.126944444 })}),...}

With the above functions we can now use the set of countries to return an
alliance along with the CountryInfoMap.
--}

fetchAllianceInfo :: Endpoint -> Name -> IO (Maybe (Alliance, CountryInfoMap))
fetchAllianceInfo url allianceName = undefined

{--
... and

>>> fetchAllianceInfo url fpda
Just (Alliance {name = "Five Power Defence Arrangements", ...}, ...)

works, returning the Alliance and the map of country infos.

So, for the bonus:

>>> let (Just (ali, cim)) = it
--}

{-- BONUS -------------------------------------------------------

Now that we have an alliance, we can build (a very simple) AllianceMap,
and we have the CountryInfoMap. This means we can display an alliance and
its countries.

Using kmlifyAlliances, on a global-viewer, show the Five Power Defence 
Arrangements.

>>> let (Just kml) = kmlifyAlliance cim (Map.fromList [(fpda, ali)]) fpda
>>> skeletonKML kml
... output saved to five-power.kml
--}
