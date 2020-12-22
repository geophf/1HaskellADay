{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D22.Exercise where

{--
Yesterday, we marshalled the data for an alliance: its member countries, their
capitals and air bases, along with associated metadata.

Wow! There's a lot of ... 'stuff' that goes into even a tiny alliance of just
five countries, like the Five Power Defence Arrangements.

Imagine if a huge country, like the USA or Russia were a member country, with
all their assets.

One won't have to imagine, for those data are there.

But back to the topic at hand.

Now that you have marshalled those data from the graph store, let's translate
those data to KML/Keyhole Markup Language, and view what this alliance looks
like, with its assets, on the globe.

First, let's get back to the state where we ended yesterday.
--}

import Graph.Query

import Y2020.M12.D21.Solution

-- But "where we ended yesterday" isn't helpful to getting to where we need
-- to be. What do we need to get there? Well, we need an Alliance from a 
-- Name, and, from that alliance, we need the AirbasesByCountry and the
-- CountryInfoMap. Let's get all those.

import qualified Y2020.M12.D18.Solution as Fetch

-- I am going to make `Fetch` happen. Today.

import Y2020.M11.D20.Solution (AirBaseByCountry)
import Y2020.M10.D30.Solution hiding (name)     -- for Alliance
import Y2020.M11.D17.Solution hiding (Capital)  -- for CountryInfo and ..-Map

import Data.Aeson.WikiDatum

type Enchilada = Maybe (Alliance, AirBaseByCountry, CountryInfoMap)

everythingFor :: Endpoint -> Name -> IO Enchilada
everythingFor url allianceName = undefined

{-- 
Okay, now that we've got everything, let's display the alliance, its member
nations, their capitals, and their air bases on a globe... unfortunately,
kmlifyAlliances doesn't work with air bases ... does it need to be modified
to do so?
--}

allianceAsKML :: Alliance -> AirBaseByCountry -> CountryInfoMap -> IO ()
allianceAsKML alliance airbases countriesInfo = undefined

{--
There you go!

Show your results.
--}

