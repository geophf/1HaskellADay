{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D20.Exercise where

{--
Okay, so we created folders, and, hypothetically, we can even create sub-
folders, if we wished, but, today, instead of building on this folder-
skeleton, we're going to focus on just one country and its airbases.

Which country?

Let's find out.

Today's Haskell problem.

Rank countries by number of their airbases. The country with the most airbases,
first. Okay, which country is that?

Now, look through this list. Pick a country that has a "reasonable" number of
airbases. "Reasonable number" is, of course, a number you're comfortable working
with. With that country, and its airbases, create:

1. a placemark of the lat/long of that country's capital. Label it.
2. placemarks for each of the airbases of that country.
3. linkages from the country's capital to each of its airbases.

Output this structure as KML, view it in a KML-viewer, such as

earth.google.com.

Share your results here.
--}

import Y2020.M10.D12.Solution    -- for Country and AirBase
import Y2020.M11.D17.Exercise    -- for CountryInfo

import Data.XHTML.KML

import Data.Map (Map)
import Data.Set (Set)

import Data.List (sortOn)
import Data.Ord                  -- for Down

type AirBaseByCountry = Map Country (Set AirBase)

{--
>>> loadBases (workingDir ++ file)
>>> let bases = it
--}

airbaseByCountry :: Map Icao AirBase -> AirBaseByCountry
airbaseByCountry = undefined

airbaseCountByCountry :: AirBaseByCountry -> [(Country,Int)]
airbaseCountByCountry = undefined

-- 1. which country has the most airbases? Which countries have no airbases?

-- 2. How many countries do have airbases?

----- AND NOW! To the mapping-on-a-globe-work!

{--
For the countries that have airbases, create a mapping of countries to their
capitals. See yesterday's exercise.

>>> readCapitals (capitalDir ++ capitalsJSON)
>>> let caps = it

Now, for a country with a 'reasonable' number of airbases, get that country,
its capital, and the airbases.
--}

data AirPower = AirPower CountryInfo (Set AirBase)
   deriving (Eq, Show)

airbasesOf :: AirBaseByCountry -> CountryInfoMap -> Country -> Maybe AirPower
airbasesOf = undefined

{--
>>> :set -XOverloadedStrings 
>>> airbasesOf abc cim "Belgium"
Just (AirPower (CI {country = WD {qid = "http://www.wikidata.org/entity/Q31",...

3. Output the XML for the country and its airbases, something like:

<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://earth.google.com/kml/2.0">
 <Document>
  <name>Belgium's Air Power</name>
  <Folder>
   <name>Belgium</name>
   <Description>It's Tuesday</Description>
   <Placemark>
    <name>Brussels</name>
    <Description>Capital</Description>
    <Point>
     <coordinates>4.351666666,50.846666666,1000.0</coordinates>
    </Point>
   </Placemark>
   <Folder>
    <name>Air Bases</name>
    <Folder>
     <name>Jehonville Air Base</name>
     <Placemark>
      <name>Jehonville Air Base</name>
      <Point>
       <coordinates>5.22389,49.89167,1000.0</coordinates>
      </Point>
     </Placemark>
     <Placemark>
      <name>Brussels - Jehonville Air Base</name>
      <LineString>
       <coordinates>
        4.351666666,50.846666666,1000.0
        5.22389,49.89167,1000.0
       </coordinates>
      </LineString>
     </Placemark>
    </Folder>
...

A sample, demonstrative, KML is located in this directory.
--}

belgianDir :: FilePath
belgianDir = "Y2020/M11/D20/"

belgianKML :: FilePath
belgianKML = "belgian-airbases.kml"

airpower2KML :: AirPower -> KML
airpower2KML = undefined

kmlify :: AirPower -> IO ()
kmlify = undefined
