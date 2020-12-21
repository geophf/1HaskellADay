{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D21.Solution where

{--
Okay, so 'yesterday' we were able to extract the countries (and their
capitals) of an alliance. Today, let's add the country's air bases. Why?

This way, we can answer the question of: "What is an alliance's air power?"
better.

Because why? ... so we can play STRATEGO! of course!

... anyone remember that game?

(I don't.)

Okay, we uploaded the air bases of the world way back in October, I believe,
but code doesn't rot, you know, it doesn't grow stale. You can look up that 
code and use it here, MONTHS LATER! JUST LIKE THAT OLD LOAF OF BREAD YOU 
BOUGHT LAST YEAR TO MAKE EGG SALAD SANDWICHES TODAY!

... okay, that metaphor went a mite too far.

VEGEMITE TOO FAR! HA!

... but, looking at the Y2020.M10.D12.Solution.AirBase-type, that's not
quite what we're looking for?

Sigh. The life of the Artiste is never a satisfied one. Eh.
--}

import Y2020.M10.D12.Solution hiding (Country, country)  -- for AirBase, ... not Country

{--
What's neat about that import is that the FromJSON-instance is not on the
AirBase-type, but on the AirBase'-type, allowing us to create a
FromJSON-instance for the AirBase-type here.

COINCIDENCE? I THINK NOT!

but...

GENIUS? I THINK NOT!

... no, ... wait ...

Okay. Anyway. Yesterday, we have the countries (and capitals) of an alliance,
today, we'll get the air bases of an alliance and marry the two. "Marry the
two, ... how?" you ask.

Glad you asked, because that's today's #haskell exercise.
--}

import Data.Aeson
import Data.Aeson.Types (Parser)

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import qualified Data.Vector as V

import Data.Aeson.WikiDatum

import Graph.Query
import Graph.JSON.Cypher

import Graph.JSON.Cypher.Read.Rows (TableRow)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2020.M10.D30.Solution hiding (name)     -- for Alliance
import Y2020.M11.D20.Solution (AirBaseByCountry)
import qualified Y2020.M11.D20.Solution as AB
import Y2020.M12.D16.Solution                   -- for Country
import qualified Y2020.M12.D18.Solution as Fetcher

{--
nupe:
instance FromJSON AirBase where
   parseJSON = withObject "Air Base" $ \v ->
      Base <$> v .: "url" <*> v .: "name" <*> v .: "icao" <*> v .: "country"
           <*> (v .: "location" >>= convertPoint)

AirBase has a country, but we're getting that externally, so HELPER TYPE!
--}

data NoCountryForOldAirBase = NCFOAB Name Name Name LongLat
   deriving (Eq, Ord, Show)

ncfoab2AirBase :: Country -> NoCountryForOldAirBase -> AirBase
ncfoab2AirBase (Country c _ _) (NCFOAB u n i ll) = Base u n i c ll

-- Javascripts `...`-operator, anyone? ... I didn't say that. (Yes, I did.)

-- and with that we can write the FromJSON instance for NCFOAB

instance FromJSON NoCountryForOldAirBase where
   parseJSON = withObject "Air Base" $ \v ->
      NCFOAB <$> v .: "url" <*> v .: "name" <*> v .: "icao"
             <*> (v .: "location" >>= convertPoint)

-- with this instance, let's grab airbases for a country from the graph-store

airbasesQuery :: Country -> Cypher
airbasesQuery (Country c _ _) =
   T.concat ["MATCH (:Country { name: '", c, "' })<-[:OF]-(b:Base) return b"]

airbasesOf :: Endpoint -> Country -> IO [AirBase]
airbasesOf url country =
   map (ncfoab2AirBase country . head . RR.row) . RR.justRows
       <$> getGraphResponse url [airbasesQuery country]

{--
>>> :set -XOverloadedStrings 
>>> graphEndpoint
...
>>> let url = it
>>> let country = Country "Malaysia" Nothing []
>>> airbasesOf url country
[Base {k = "http://www.wikidata.org/entity/Q7276970", val = "RMAF Gong Kedak", 
       icao = "WMGK", country = "Malaysia", 
       pos = point({ latitude: 5.798888888, longitude: 102.490277777 })},
 Base {k = "http://www.wikidata.org/entity/Q1551466", val = "RMAF Butterworth",
       icao = "WMKB", country = "Malaysia", 
       pos = point({ latitude: 5.466111111, longitude: 100.391111111 })},
 Base {k = "http://www.wikidata.org/entity/Q1082819", val = "Simpang Airport", 
       icao = "WMKF", country = "Malaysia", 
       pos = point({ latitude: 3.112222222, longitude: 101.7025 })},
 Base {k = "http://www.wikidata.org/entity/Q3235616", 
       val = "Sultan Haji Ahmad Shah Airport", 
       icao = "WMKD", country = "Malaysia", 
       pos = point({ latitude: 3.775175, longitude: 103.208572222 })}]

Okay, that's great!

But. Also, we want to have just one query give us sets of air bases from a
set of countries. To do that, we'll need another intermediary.
--}

airbasesFor :: Endpoint -> Alliance -> IO AirBaseByCountry
airbasesFor url alliance =
   countryAirBaseMap . map (head . RR.row) . RR.justRows
       <$> getGraphResponse url [airbasesWithCountryQuery alliance]

{--
The query for this airbasesFor-function is just slightly expanded from
the airbasesOf-function.
--}

airbasesWithCountryQuery :: Alliance -> Cypher
airbasesWithCountryQuery alliance =
   T.concat ["MATCH p=(c:Country)<-[:OF]-(:Base) WHERE c.name IN ",
             T.pack (show . Set.toList $ countries alliance), " RETURN p"]

{--
Recall from yesterday we can get an alliance (and the associated CountryInfoMap,
which will be important for airbase-mapping on a globe), with:

>>> Fetcher.fetchAllianceInfo url fpda 
Just (Alliance {name = "Five Power Defence Arrangements", aliases =...})
>>> let (Just (ali, cim)) = it

So, with that, we can query for the airbases of an alliance:

>>> getGraphResponse url [airbasesWithCountryQuery ali]
...\"errors\":[]}\n"
>>> let resp = it
>>> length resp
68655

... not bad, ... but it is a small alliance, member-nation-wise.

A sample row element is an array of values:
--}

sampleCountryAirBaseRow :: String
sampleCountryAirBaseRow = 
   concat ["[{\"aliases\":[\"UK\"],\"name\":\"United Kingdom\",",
           "\"qid\":\"http://www.wikidata.org/entity/Q145\"},{},",
           "{\"name\":\"Goose Green Airfield\",\"icao\":\"SFDW\",",
           "\"location\":{\"type\":\"Point\",\"coordinates\":[-58.97666667,",
           "-51.82388889],\"crs\":{\"srid\":4326,\"name\":\"wgs-84\",",
           "\"type\":\"link\",\"properties\":",
           "{\"href\":\"http://spatialreference.org/ref/epsg/4326/ogcwkt/\",",
           "\"type\":\"ogcwkt\"}}},\"url\":",
           "\"http://www.wikidata.org/entity/Q16527287\"}]"]

-- which is a mouthful, but we have already parsed both Country and, now,
-- AirBase from this sea of text, so now we have to marry those parsers.

-- "How do we marry these disparate parsers?" you ask. GLAD YOU ASKED!

{--
First of all, this works:

>>> (decode $ BL.pack sampleCountryAirBaseRow) :: Maybe [Value]
Just [Object (fromList [("aliases",Array [String "UK"]), ...]),
      Object (fromList []),
      Object (fromList [("location", ...)])]

So we have the three objects: the country, the relation (designated by the
empty object), and the AirBase.

So, this?
--}

data CountryAirBaseRel = CAR Country AirBase
   deriving (Eq, Ord, Show)

countryAirBaseRow :: [Value] -> Parser CountryAirBaseRel
countryAirBaseRow row = parseJSON (head row) >>= \country ->
   CAR country <$> (ncfoab2AirBase country <$> parseJSON (last row))

-- ... so does that mean:

instance FromJSON CountryAirBaseRel where
   parseJSON = withArray "Country-Air Base row" $ \row ->
                      countryAirBaseRow (V.toList row)

{--
>>> (RR.justRows resp) :: [TableRow [CountryAirBaseRel]]
[TR {row = [CAR (Country {country = "Australia", qid = Just ...})]}, ...]

Yes. Yes, it does.

And, from that, our map falls out naturally:
--}

countryAirBaseMap :: [CountryAirBaseRel] -> AirBaseByCountry
countryAirBaseMap =
   foldr inserter Map.empty . map (\(CAR (Country c _ _) a) -> (c,a))
      where inserter (c,a) = Map.insert c . setIns a . Map.lookup c <*> id
            setIns = maybe . Set.singleton <*> Set.insert

-- now write a function that will print out one of the country-airbase rows

airbasesPrinter :: Name -> Set AirBase -> IO ()
airbasesPrinter country airbases =
   putStrLn (T.unpack country)                             >>
   putStrLn (replicate 55 '-')                             >>
   mapM_ (putStrLn . T.unpack . val) (Set.toList airbases)

{--
>>> airbasesFor url ali
...
>>> let abc = it
>>> uncurry airbasesPrinter . head . tail $ Map.toList abc

Malaysia
-------------------------------------------------------
Simpang Airport
RMAF Butterworth
Sultan Haji Ahmad Shah Airport
RMAF Gong Kedak

... and thus you have countries and airbases from the graph.

And now we have all the individual pieces of an alliance, its countries,
their capitals, and all the air bases therein, we can now map that using
a KML-representation. We will do this tomorrow.

YUS!
--}
