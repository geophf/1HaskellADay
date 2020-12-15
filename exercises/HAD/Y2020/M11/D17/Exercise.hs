{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D17.Exercise where

{--
So.

Okay.

I was going to go all: map countries and their airbases to a KML-file, but
to do that, I need a lat-long of the country, and where best to do that,
other than the country's capital. But we don't have country capitals, so
it's back to wikidata to get those data.

The PROBLEM with wikidata, see: is that it's wikidata, or messy as all get-out.

So, today, we're going to get these wikidata, then clean them up and transform
them to enhanced information on countries that we can use.

Here we go.

First, here is the query that gets us these data. There are some weirditudes.

# Countries and capitals
SELECT ?country ?countryLabel ?capital ?capitalLabel ?latlongLabel
WHERE 
{
  ?capital wdt:P31 wd:Q5119.
  ?capital wdt:P1376 ?country.
  ?capital wdt:P625 ?latlong.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}

n.b.: we do not constrain countries, because, if we do, we lose countries,
like Belgium. Let's sieve these data through the lens of the countries we
already have.
--}

import Data.Aeson
import Data.Aeson.WikiDatum

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)
import Data.Set (Set)

import Data.Relation

import Graph.Query

import Y2020.M10.D12.Exercise hiding (LongLat)  -- WAAY back when for countries

data CountryInfo = CI { country :: WikiDatum, 
                        capital :: WikiDatum, 
                        latLong :: LongLat }
   deriving (Eq, Show)

instance FromJSON CountryInfo where
    parseJSON = undefined

samp :: ByteString
samp = BL.concat ["{\"country\":\"http://www.wikidata.org/entity/Q31\"",
       ",\"countryLabel\":\"Belgium\",\"capital\":\"http://www.wikidata.org/",
       "entity/Q239\",\"capitalLabel\":\"Brussels\",\"latlongLabel\":\"Point",
       "(4.351666666 50.846666666)\"}"]

{--
>>> (decode samp) :: Maybe CountryInfo
Just (CI {country = WD {qid = "http://www.wikidata.org/entity/Q31", 
                        name = "Belgium"}, 
          capital = WD {qid = "http://www.wikidata.org/entity/Q239", 
                        name = "Brussels"}, 
          latLong = point({ latitude: 50.846666666, longitude: 4.351666666 })})

WOOT!

With this instance declaration, read in the file of ... "countries" and their
capitals.
--}

capitalDir :: FilePath
capitalDir = "Y2020/M11/D17/"

capitalsJSON :: FilePath
capitalsJSON = "things-capitals.json"

type CountryInfoMap = Map Country CountryInfo

readCapitals :: FilePath -> IO CountryInfoMap
readCapitals = undefined

-- How many country-infos thingies are there?

{-- 
2. Now, load the airbase map and `capitalize` it. But we don't want to do that.
What we want to do is filter out the `countries` that don't have airbases.
Let's do that, instead.

>>> loadBases (workingDir ++ file)
>>> let bases = it
>>> let mappedBases = byICAO bases
--}

countrySet :: Map Icao AirBase -> Set Country
countrySet = undefined

{--
Now we have a set of countries-that-are-countries by which we may identify
the things in the `CountryInfoMap` that are countries and not just things.
--}

{-- BONUS -------------------------------------------------------

3. Upload Countries' Q-id's, their capitals (with their q-id's), and their
   lattness-longness to the graph data store. That means, of course, you must
   model these relations as ... well: ... relations.

--}

data CC = Cntry WikiDatum | Capitol WikiDatum LongLat 
   deriving Eq                       -- finally I spell Capitol ... correctly

instance Node CC where
   asNode = undefined

data CapAt = CAPITOL | AT
   deriving (Eq, Ord, Show)

instance Edge CapAt where
   asEdge = undefined

countryCapitalCoordinates :: CountryInfo -> Relation CC CapAt CC
countryCapitalCoordinates = undefined

-- with `countryCapitalCoordinates` you can cyph your data to the graph.

-- I recommend doing an update to countries to add their Q-id's first.
