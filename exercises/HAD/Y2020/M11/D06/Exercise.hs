{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D06.Exercise where

{--
Okay, so we parsed in 42 alliances of the world, ...

... Douglas Adams would be pleased.

... but we missed the EU, the UN, and the OA

These TLAs are killin' me, mang.

No, ... wait ...

So, let's parse them in one at a time. Do we grab them from wikidata.org?

Yes, and no. wikidata.org has the EU, but not the member states of the UN nor
the OA. So, let's do the EU today and do the other alliances on following days.

*ahem*

It would be NICE if wikidata.org did have and did regularize these data sets.
It would be NICE if somebody did that, so we didn't have to do these specialized
parse exercises.

*everybody stares at me: Well, geophf, when are you going to enter these data
into wikidata.org, now that you have them?

me: wut.

The EU comes from wikidata.org as JSON, and we have an Alliance structure.

Let's do this.
--}

import Y2020.M10.D28.Exercise hiding (Alliance) -- for WikiDatum
import Y2020.M10.D30.Exercise                   -- for everything alliance-y
import Y2020.M11.D05.Solution (todoPrep)        -- for the updated alliance-parse

import Data.Aeson

data MemberNation = Member { nation :: WikiDatum, kind :: WikiDatum }
   deriving (Eq, Ord, Show)

instance FromJSON MemberNation where
   parseJSON = withObject "Member Nation" $ \v ->
      Member <$> v *: "country" <*> v *: "kind"

euDir :: FilePath
euDir = "Y2020/M11/D06/"

eu :: FilePath
eu = "eu.json"

{--
These data are the result of this SPARQL query:

# European Union member countries
SELECT ?country ?countryLabel ?kind ?kindLabel
WHERE 
{
  wd:Q458 wdt:P150 ?country.
  ?country wdt:P31 ?kind.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
--}

parseEU :: FilePath -> IO Alliance
parseEU = undefined

-- How many members of the EU are there? Please remember to remove duplicates.

addEU :: FilePath -> AllianceMap -> IO AllianceMap
addEU = undefined

-- You'll note that the EU has wikidata Q-identifiers ... we may, at some
-- future time, wish to update our graph (do you remember our graph?) with
-- these identifiers. ... but that's a story to tell for another day.
