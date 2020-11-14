{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D16.Exercise where

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

import Y2020.M10.D12.Exercise    -- for Country and AirBase
import Y2020.M11.D13.Exercise    -- for KML-output

import Data.XHTML

import Data.Map (Map)
import Data.Set (Set)

import Data.List (sortOn)
import Data.Ord                  -- for Down

type AirBaseByCountry = Map Country (Set AirBase)

airbaseByCountry :: Map Icao AirBase -> AirBaseByCountry
airbaseByCountry = undefined

airbaseCountByCountry :: AirBaseByCountry -> [(Country,Int)]
airbaseCountByCountry = undefined

-- 1. which country has the most airbases?

kmlifyAirbasesOf :: AirBaseByCountry -> Country -> IO ()
kmlifyAirbasesOf = undefined
