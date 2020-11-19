{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D12.Solution where

import Control.Arrow ((&&&), first)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)

import Data.Aeson
import Data.Aeson.WikiDatum

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

type Key = Text
type Entity = Text
type Icao = Text
type Country = Text

{--
Moved to Data.Aeson.WikiDatum

data LongLat = Point { lon :: Double, lat :: Double }
   deriving (Eq, Ord)

instance Show LongLat where
   show (Point lon lat) = "point({ latitude: " ++ show lat ++ ", longitude: "
                       ++ show lon ++ " })"
--}

data AirBase = Base { k       :: Key,
                      val     :: Entity,
                      icao    :: Icao,
                      country :: Country,
                      pos     :: LongLat }
   deriving (Eq, Ord, Show)

-- 1. Load in the airbases.

data Airbase' = B' Key Entity Icao Country String

instance FromJSON Airbase' where
   parseJSON = withObject "Base" $ \v -> B'
       <$> v .: "item"
       <*> v .: "itemLabel"
       <*> v .: "icao"
       <*> v .: "countryLabel"
       <*> v .: "loc"

ab2ab :: Airbase' -> [AirBase]   -- a simpler approach would be to use (@:)
ab2ab (B' k e i c s) = Base k e i c <$> ll s

{--
Moved to Data.Aeson.WikiDatum

-- we parse: "Point(28.2125 47.8625)"
ll :: String -> [LongLat]

-- "Point" +++ = -- yeah, I'm writing a String DCG in Haskell, wheeeeeeee!

ll str = maybe [] ll' (stripPrefix "Point(" str)

>>> :t fromMaybe
fromMaybe :: a -> Maybe a -> a
>>> :t maybe
maybe :: b -> (a -> b) -> Maybe a -> b

ll' :: String -> [LongLat]
ll' str = reads str            >>= \(lat, r:est) ->
          reads est            >>= \(lon, _)     ->
          return (Point lat lon)
--}

loadBases :: FilePath -> IO [AirBase]
loadBases fil = BL.readFile fil >>= \f -> 
                return (maybe [] (concat . map ab2ab) (decode f))

-- loadBases f = maybe (return []) (concat . map ab2ab) (decode <$> BL.readFile f) -- nah.

-- ... let's try this on a sample first:

sample :: ByteString
sample = BL.pack ("{\"item\":\"http://www.wikidata.org/entity/Q1031458\","
               ++ "\"itemLabel\":\"Mărculești Air Force Base\",\"icao\":"
               ++ "\"LUBM\",\"countryLabel\":\"Moldova\",\"loc\":\"Point"
               ++ "(28.2125 47.8625)\"}")

{--
>>> ab2ab <$> decode sample
Just [Base "http://www.wikidata.org/entity/Q1031458"
           "M\ETXrcule\EMti Air Force Base" "LUBM" "Moldova" 
           (Point 28.2125 47.8625)]

WHEW! ... so, in that case:

>>> loadBases (workingDir ++ file)
...

>>> let bases = it

-- 2. How many airbases are there?

>>> length bases
1175

--}

-- 3. Actually, that's a tricky question, because, as you examine the data, 
--    you see there are duplicate results. And a `distict` in the SPARQL
--    query doesn't eliminate the duplicates in the result.

-- Let's solve that problem.

byICAO :: [AirBase] -> Map Icao AirBase
byICAO = Map.fromList . map (first icao . dup)
   where dup = id &&& id

-- 4. Now: how many bases are there, without duplicates?

{--
>>> let mappedBases = byICAO bases
>>> Map.size mappedBases 
921

>>> take 3 (Map.elems mappedBases)
[Base {k = "http://www.wikidata.org/entity/Q5372593", 
       val = "Emirau Airport", icao = "AYEE", country = "Papua New Guinea", 
       pos = Point {lon = 149.975, lat = -1.64166667}},
 Base {k = "http://www.wikidata.org/entity/Q3565073", 
       val = "Natal Air Force Base", icao = "BANT", country = "Brazil", 
       pos = Point {lon = -35.2475, lat = -5.91138889}},
 Base {k = "http://www.wikidata.org/entity/Q940576", 
       val = "Thule Air Base", icao = "BGTL", country = "Greenland", 
       pos = Point {lon = -68.703333, lat = 76.531389}}]

My dad served at the Thule Air Base when he was in the Strategic Air Command
with the US Air Force back in the 1960s.

Also, n.b.: they put the longitude first?
--}
