{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D22.Solution where

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

import qualified Data.Map as Map

import Data.Maybe (maybeToList)

import Data.XHTML.KML

import Graph.Query

import qualified Y2020.M12.D21.Solution as ABC

-- But "where we ended yesterday" isn't helpful to getting to where we need
-- to be. What do we need to get there? Well, we need an Alliance from a 
-- Name, and, from that alliance, we need the AirbasesByCountry and the
-- CountryInfoMap. Let's get all those.

import qualified Y2020.M12.D18.Solution as Fetch

-- I am going to make `Fetch` happen. Today.

import Y2020.M11.D20.Solution (AirBaseByCountry)
import Y2020.M10.D30.Solution hiding (name)     -- for Alliance
import Y2020.M11.D17.Solution hiding (Capital)  -- for CountryInfo and ..-Map
import qualified Y2020.M11.D23.Solution as A2K
import Y2020.M12.D16.Solution (fpda)

import Data.Aeson.WikiDatum

type Enchilada = (Alliance, AirBaseByCountry, CountryInfoMap)

everythingFor :: Endpoint -> Name -> IO (Maybe Enchilada)
everythingFor url allianceName =
   Fetch.fetchAllianceInfo url allianceName >>= dude url

dude :: Endpoint -> Maybe (Alliance, CountryInfoMap) -> IO (Maybe Enchilada)
dude _ Nothing = -- Dude. Seriously?
   return Nothing
dude url (Just (ali, cim)) =
   ABC.airbasesFor url ali >>= \abf ->
   return $ Just (ali, abf, cim)

{-- 
>>> graphEndpoint 
...
>>> let url = it
>>> everythingFor url fpda
Just ...
>>> let (Just tamale) = it

Okay, now that we've got everything, let's display the alliance, its member
nations, their capitals, and their air bases on a globe... unfortunately,
kmlifyAlliances doesn't work with air bases ... does it need to be modified
to do so?

Well, I think this is a rather straightforward application of the continuation-
passing style. Instead of straight-up calling kmlifyAlliance, we pass in a

(Country -> KML) ... or better: (Point -> KML elements) ... because that's 
what we care about.

function, f, to the CPS-version of kmlifyAlliance and have f do the work of
adding the airbases to each country-folder, and that function simply uses
the work we did on ... 'Tuesday' (Belgium's air bases. Keep up, fam.)

Unfortunately, I have to rewrite the follow-on function:

Y2020.M11.D20.Solution.airpower2KML

... because it's comically Belgium-centric, but such is life, and I can reuse
the helper-functions, so there's that.

Given all these constraints, does CPS make all that much sense? I don't know.

--}

-- All that discussion above talks to the implementation of allianceAsKML.

allianceAsKML :: Enchilada -> IO ()
allianceAsKML (alliance, airbases, countriesInfo) =
   skeletonKML (alliances2kml' countriesInfo airbases [alliance])

{--
I think what we need to do here is to rewrite the alliances2kml function in
CPS, so that it works for the original usage, then call that CPS function.
--}

alliances2kml' :: CountryInfoMap -> AirBaseByCountry -> [Alliance] -> KML
alliances2kml' cim abc =
   KML "World Alliances" . map (A2K.alliance2folder' cim abc)

{--
alliance2folder' :: CountryInfoMap -> AirBaseByCountry -> Alliance -> Key
alliance2folder' = undefined

-- actually, this belongs in the Y2020.M11.D23.Solution module. ... DONE!

So:

>>> allianceAsKML tamale
<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://earth.google.com/kml/2.0">
 <Document>
  <name>
   World Alliances
  </name>
...
</kml>

et voilà! That Enchilada is one Hot Tamale! ;)
--}
