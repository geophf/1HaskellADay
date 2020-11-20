{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D20.Solution where

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

import Y2020.M10.D12.Solution (Country, AirBase, Icao)
import qualified Y2020.M10.D12.Solution as A

import Y2020.M11.D17.Solution    -- for Country capitals

import Data.Aeson.WikiDatum
import Data.XHTML.KML

import Control.Arrow ((&&&))

import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust)

import Data.Ord                  -- for Down

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

type AirBaseByCountry = Map Country (Set AirBase)

{--
>>> A.loadBases (A.workingDir ++ A.file)
>>> let bases = A.byICAO it
--}

airbaseByCountry :: Map Icao AirBase -> AirBaseByCountry
airbaseByCountry = foldr inserter Map.empty . Map.elems
   where inserter ab = Map.insert (A.country ab) . inserterer ab <*> id

inserterer :: AirBase -> AirBaseByCountry -> Set AirBase
inserterer ab = maybe Set.empty (Set.insert ab) . Map.lookup (A.country ab)

airbaseCountByCountry :: AirBaseByCountry -> [(Country,Int)]
airbaseCountByCountry = sortOn (Down . snd) . Map.toList . Map.map Set.size

{--
1. which country has the most airbases? Which countries have no airbases?

>>> let abc = airbaseByCountry bases
>>> take 3 (airbaseCountByCountry abc)
[("United States of America",141),("United Kingdom",81),("Germany",62)]

>>> map fst $ filter ((== 0) . snd) (airbaseCountByCountry abc)
["Algeria","Argentina","Austria","Bahrain","Bolivia","Bosnia and Herzegovina",
 "Djibouti","Dominican Republic","Ecuador","El Salvador","Ethiopia","Georgia",
 "Greenland","Honduras","Kazakhstan","Lebanon","Myanmar","Niger","Panama",
 "Papua New Guinea","Qatar","Saint Helena, Ascension and Tristan da Cunha",
 "Slovakia","Sudan","Tanzania","Tunisia","Turkish Republic of Northern Cyprus",
 "Tuvalu","Uruguay","Yugoslavia","Zimbabwe"]

2. How many countries do have airbases?

>>> length $ filter ((> 0) . snd) (airbaseCountByCountry abc)
77
--}

----- AND NOW! To the mapping-on-a-globe-work!

{--
For the countries that have airbases, create a mapping of countries to their
capitals. See yesterday's exercise.

>>> readCapitals (capitalDir ++ capitalsJSON)
>>> let cim = it

Now, for a country with a 'reasonable' number of airbases, get that country,
its capital, and the airbases.
--}

data AirPower = AirPower CountryInfo (Set AirBase)
   deriving (Eq, Show)

airbasesOf :: AirBaseByCountry -> CountryInfoMap -> Country -> Maybe AirPower
airbasesOf abc cim c = AirPower <$> Map.lookup c cim <*> Map.lookup c abc

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

sname :: WikiDatum -> String
sname = T.unpack . name

airpower2KML :: AirPower -> KML
airpower2KML (AirPower (CI country cap latlong) airbases) = 
   KML "Belgium's Air Power"
       [F (Folder (sname country) (Just "It's Tuesday")
                  [place cap latlong, foldAirs cap latlong airbases])]

foldAirs :: WikiDatum -> LongLat -> Set AirBase -> Key
foldAirs cap latlong =
    F . Folder "Air Bases" Nothing 
      . map (airbnb cap latlong) . Set.toList

place :: WikiDatum -> LongLat -> Key
place cap = P . Placemark (sname cap) (Just "Capital") . return . Pt . pt

pt :: LongLat -> Point
pt ll = Coord (lat ll) (lon ll) 1000.0

airbnb :: WikiDatum -> LongLat -> AirBase -> Key
airbnb capw ll ab = 
   let airname = T.unpack $ A.val ab
       p n = P . Placemark n Nothing . return
       cap = sname capw
   in  F . Folder airname Nothing
         $ [p airname . Pt $ pt (A.pos ab), 
            p (concat [cap, " - ", airname]) $ line ll ab]

line ::  LongLat -> AirBase -> PointOrLine
line ll = Ln . Line . map pt . (ll:) . return . A.pos

-- and with that, you should be able to KMLify the Airbases-as-KML

kmlify :: AirPower -> IO ()
kmlify = skeletonKML . airpower2KML

belgium :: IO ()
belgium = A.loadBases (A.workingDir ++ A.file)      >>= \b0 ->
          readCapitals (capitalDir ++ capitalsJSON) >>= \cim ->
          let bases = A.byICAO b0
              abc = airbaseByCountry bases
              belgAirbases = fromJust (airbasesOf abc cim "Belgium")
          in kmlify belgAirbases

-- ... and: BOOM! It's Tuesday! ;)
