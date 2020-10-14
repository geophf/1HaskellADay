{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D13.Solution where

{--
Yesterday, we translated the results of a SPARQL query into a set of AirBase
values. Today, let's examine these data in a graph database.

Let's look at the relationship between airbases and countries today.

... no.

I'm still unhappy that I had to write a parser for the point-type in the
point-as-string encoded into the JSON, so today, we're going to do something
different. We're going to write out the data as JSON, but we're really going
to write out JSON, and not types-as-strings JSON that really gets my goat.

Moo.

... or something like that.

Then we'll look at countries-continents-as-a-service, but tomorrow, not today.
--}

import Y2020.M10.D12.Solution

import Data.Aeson

-- import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

instance ToJSON AirBase where
   toJSON (Base k v i c p) =
      object ["key" .= k, "entity" .= v, "icao" .= i,
              "country" .= c, "location" .= p]

instance ToJSON LongLat where
   toJSON (Point lon lat) = object ["longitude" .= lon, "latitude" .= lat]

-- and don't get me started on why "Point(lon lat)" has longitude first, unlike
-- the rest of the Milky Way galaxy but Wikidata? NOOOOOOOOO! ... and if it
-- were formatted as JSON (and not as JSON-as-a-string) it wouldn't be a 
-- problem ... AT ALL ... but was it? I ASK YOU!

writeJSON :: FilePath -> [AirBase] -> IO ()
writeJSON output = BL.writeFile output . encode

{--
>>> loadBases (workingDir ++ file)
>>> let bases = it
>>> let mappedbases = byICAO bases
>>> writeJSON "Y2020/M10/D13/airbasesLatsLongs.json" (Map.elems mappedbases)

whew! I feel much better now. Thank you.
--}
