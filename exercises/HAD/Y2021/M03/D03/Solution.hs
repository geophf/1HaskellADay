{-# LANGUAGE OverloadedStrings #-}

module Y2021.M03.D03.Solution where

{--
Okay, so we now have some wineries with geo-locations!

Let's geo-locationitize them.

(That's a word now.)
--}

-- first, let's grab them geo-located (that's a word now) wineries

import Data.Aeson

import Data.Maybe (mapMaybe)

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
toGeoWinery [i,n,q,lon,lat] =
   RR.fromJSON1 i >>= \i' ->
   RR.fromJSON1 n >>= \n' ->
   RR.fromJSON1 q >>= \q' ->
   RR.fromJSON1 lon >>= \lon' ->
   RR.fromJSON1 lat >>= \lat' ->
   return (GeoWinery i' (WD q' n') (Point lon' lat'))

fetchGeoWineries :: Endpoint -> IO [GeoWinery]
fetchGeoWineries url =
   mapMaybe (toGeoWinery . RR.row) . RR.justRows
      <$> getGraphResponse url [geowineriesQuery]

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
plotGeoWineries = KML "Wineries around the world!" . map toKMLwinery

toKMLwinery :: GeoWinery -> Key
toKMLwinery (GeoWinery _id wd loc) =
   F (Folder (str name) (Just (concat ["wikidata QName: ", str qid]))
     [P (Placemark (str name) Nothing [Pt (Coord (lat loc) (lon loc) 5)])])
     where str f = T.unpack (f wd)

-- show your results on google earth or another KML-renderer! :D 

{--
>>> skeletonKML (plotGeoWineries gwins)
<kml xmlns="http://earth.google.com/kml/2.0">
 <Document>
  <name>
   Wineries around the world!
  </name>
  <Folder>
   <name>
    Castello di Amorosa
   </name>
   <Description>
    wikidata QName: http://www.wikidata.org/entity/Q1913305
   </Description>
   <Placemark>
    <name>
     Castello di Amorosa
    </name>
    <Point>
     <coordinates>
      -122.5427,38.5584,5.0
     </coordinates>
    </Point>
   </Placemark>
...
  </Folder>
 </Document>
</kml>

... moved to wineries.kml
--}
