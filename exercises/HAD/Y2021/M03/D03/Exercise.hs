{-# LANGUAGE OverloadedStrings #-}

module Y2021.M03.D03.Exercise where

{--
Okay, so we now have some wineries with geo-locations!

Let's geo-locationitize them.

(That's a word now.)
--}

-- first, let's grab them geo-located (that's a word now) wineries

import Data.Aeson

import qualified Data.Text as T

import Data.XHTML.KML

import Graph.Query
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Aeson.WikiDatum

import Y2021.M01.D21.Solution (Idx)

data GeoWinery = GeoWinery { idx :: Idx,
                             winery :: WikiDatum,
                             location :: LongLat }
   deriving (Eq, Ord, Show)

geowineriesQuery :: Cypher
geowineriesQuery =
   T.pack (unwords ["MATCH (w:Winery)",
                    "WHERE w.location IS NOT NULL",
                    "RETURN id(w), w.name, w.qid, w.location.longitude,",
                    "w.location.latitude"])

toGeoWinery :: [Value] -> Maybe GeoWinery
toGeoWinery = undefined

fetchGeoWineries :: Endpoint -> IO [GeoWinery]
fetchGeoWineries = undefined

{--
>>> graphEndpoint >>= fetchGeoWineries
[GeoWinery {idx = 494, 
            winery = WD {qid = "http://www.wikidata.org/entity/Q1913305", 
                         name = "Castello di Amorosa"}, 
            location = point({ latitude: 38.5584, longitude: -122.5427 })}, ...]
>>> let gwins = it
>>> length gwins
140
--}

-- now that we got'm, let's plot'm!

plotGeoWineries :: [GeoWinery] -> KML
plotGeoWineries = undefined

-- show your results on google earth or another KML-renderer! :D 
