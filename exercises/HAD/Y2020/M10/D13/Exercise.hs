{-# LANGUAGE OverloadedStrings #-}

module Y2020.M10.D13.Exercise where

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

import Y2020.M10.D12.Exercise

import Data.Aeson

instance ToJSON AirBase where
   toJSON = undefined

instance ToJSON LongLat where
   toJSON = undefined

-- and don't get me started on why "Point(lon lat)" has longitude first, unlike
-- the rest of the Milky Way galaxy but Wikidata? NOOOOOOOOO! ... and if it
-- were formatted as JSON (and not as JSON-as-a-string) it wouldn't be a 
-- problem ... AT ALL ... but was it? I ASK YOU!

writeJSON :: FilePath -> [AirBase] -> IO ()
writeJSON = undefined

-- whew! I feel much better now. Thank you.
