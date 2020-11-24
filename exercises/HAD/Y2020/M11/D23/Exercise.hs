{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D23.Exercise where

{--

'Yesterday' we created a KML-file of a country and its airbases, so we know
a country's air power (kinda). But we don't know the 'regional' air power:
what other countries come into play locally and globally? That's where
alliances come into the picture.

Today, we're going to map out the countries of an alliance in KML so that
it can be represented on earth.google.com or some other earth-map-viewer.

Recall that we could position a country on its capital:
--}

import qualified Y2020.M11.D17.Solution as Capitals

-- ... and we also built out countries in alliances:

import qualified Y2020.M11.D10.Solution as Alliances
import Y2020.M10.D28.Solution (Name)

-- ... and we now have a way to map to KML from values:

import Data.XHTML.KML

import Y2020.M10.D30.Exercise     -- for Alliance

-- with these piece, please pick an alliance and KMLify it, its countries,
-- and the countries' capitals.

kmlifyAlliance :: AllianceMap -> Name -> Maybe KML
kmlifyAlliance = undefined

-- with that KMLified alliance, you should be able to output that as XML, using
-- the KML-library.

-- post a picture of your alliance in this exercise's replies.
