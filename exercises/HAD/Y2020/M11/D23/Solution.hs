{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D23.Solution where

{--

'Yesterday' we created a KML-file of a country and its airbases, so we know
a country's air power (kinda). But we don't know the 'regional' air power:
what other countries come into play locally and globally? That's where
alliances come into the picture.

Today, we're going to map out the countries of an alliance in KML so that
it can be represented on earth.google.com or some other earth-map-viewer.

Recall that we could position a country on its capital:
--}

import Y2020.M11.D17.Solution (CountryInfo, CountryInfoMap, CountryInfo(CI))
import qualified Y2020.M11.D17.Solution as Capitals

-- ... and we also built out countries in alliances:

import qualified Y2020.M11.D10.Solution as Alliances

-- ... and we now have a way to map to KML from values:

import Data.XHTML.KML
import Data.Aeson.WikiDatum            -- for LongLat

import Y2020.M10.D30.Solution hiding (name)     -- for Alliance
import Y2020.M10.D12.Solution (Country)
import Y2020.M11.D10.Solution (go)              -- for the AllianceMap

import Control.Arrow ((***))
import Control.Monad (join)

import Data.List (genericLength,intercalate)

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

-- with these piece, please pick an alliance and KMLify it, its countries,
-- and the countries' capitals.

kmlifyAlliance :: CountryInfoMap -> AllianceMap -> Name -> Maybe KML
kmlifyAlliance cim am n = alliances2kml cim . return <$> Map.lookup n am

tstr :: Text -> String
tstr = T.unpack

alliances2kml :: CountryInfoMap -> [Alliance] -> KML
alliances2kml cim = KML "World Alliances" . map (alliance2folder cim)

alliance2folder :: CountryInfoMap -> Alliance -> Key
alliance2folder cim (Alliance name aliases countries) =
   let cent = centroid name cim countries
       kent = P (Placemark (tstr name) Nothing [Pt cent])
   in  F (Folder (tstr name) (alis aliases)
                 (kent:countryCap cent cim countries))

centroid :: Name -> CountryInfoMap -> Set Country -> Point
centroid alliance cim = cent . set2countryInfosSomthing Capitals.latLong cim

ll2coord :: LongLat -> Point
ll2coord = tup2Pt . longlat2tup

cent :: [LongLat] -> Point
cent = tup2Pt . join (***) avg . unzip . map longlat2tup

longlat2tup :: LongLat -> (Double, Double)
longlat2tup (Point ln lt) = (ln, lt)

tup2Pt :: (Double, Double) -> Point
tup2Pt (a,b) = Coord b a 5.0

avg :: Fractional a => [a] -> a
avg = (/) . sum <*> genericLength

countryCap :: Point -> CountryInfoMap -> Set Country -> [Key]
countryCap = set2countryInfosSomthing . countryFolder

countryFolder :: Point -> CountryInfo -> Key
countryFolder pt (CI cntry cap ll) =
   let namei = tstr (name cap)
       cname = tstr (name cntry)
       capPt = ll2coord ll
   in  F (Folder cname Nothing (map (P . Placemark namei Nothing . return)
                 [Pt capPt, Ln (Line [capPt, pt])]))

set2countryInfosSomthing :: (CountryInfo -> a)
                         -> CountryInfoMap -> Set Country -> [a]
set2countryInfosSomthing f cim =
   mapMaybe (\c -> f <$> Map.lookup c cim) . Set.toList

alis :: Set Alias -> Maybe Description
alis ali | Set.size ali == 0 = Nothing
         | otherwise         = Just ("Alliance aliases: "
    ++ tstr (T.intercalate ", " (Set.toList ali)))

-- with that KMLified alliance, you should be able to output that as XML, using
-- the KML-library.

-- post a picture of your alliance in this exercise's replies.

-- Okay, we need the CountryInfoMap and we need the alliance map (of countries)

{--
>>> Capitals.readCapitals (Capitals.capitalDir ++ Capitals.capitalsJSON)
...
>>> let caps = it
>>> go
...
>>> let alliances = it

Which alliances have Belgium in them?

>>> let nameOf (Alliance n _ _) = n
>>> map nameOf . filter (Set.member "Belgium" . countries) $ Map.elems alliances
["European Union","North Atlantic Treaty Organization","United Nations"]

... so, let's do NATO
--}

nato :: IO ()
nato =
   Capitals.readCapitals (Capitals.capitalDir ++ Capitals.capitalsJSON) >>= \caps ->
   go >>= \alliances ->
   let kml = kmlifyAlliance caps alliances "North Atlantic Treaty Organization"
   in  maybe (pure ()) skeletonKML kml
