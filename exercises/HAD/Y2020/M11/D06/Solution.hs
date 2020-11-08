{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D06.Solution where

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

import Y2020.M10.D12.Solution               -- for Country
import Y2020.M10.D28.Solution (WikiDatum, (*:), name)
import Y2020.M10.D30.Solution (Alliance(Alliance), AllianceMap, dear, moderns)
import qualified Y2020.M10.D30.Solution as A
import Y2020.M11.D05.Solution (todoPrep)    -- for the updated alliance-parse

import Control.Arrow ((&&&))

import Data.Aeson

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Map as Map

import Data.Maybe (fromJust)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

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
parseEU file = BL.readFile file >>=
   return . Alliance "European Union" Set.empty . convertNationsJSON

-- we throw an error if this file doesn't parse.

convertNationsJSON :: ByteString -> Set Country
convertNationsJSON =
   Set.fromList . map (name . nation) . fromJust . convertNationsFile

convertNationsFile :: ByteString -> Maybe [MemberNation]
convertNationsFile = decode

-- How many members of the EU are there? Please remember to remove duplicates.

{--
>>> parseEU (euDir ++ eu)
Alliance {name = "European Union", aliases = fromList [], 
          countries = fromList ["Austria","Belgium","Bulgaria","Croatia",
               "Cyprus","Czech Republic","Denmark","Estonia","Finland","France",
               "Germany","Greece","Hungary","Ireland","Italy","Latvia",
               "Lithuania","Luxembourg","Malta","Netherlands","Poland",
               "Portugal","Romania","Slovakia","Slovenia","Spain","Sweden"]}

>>> let eur = it
>>> Set.size $ A.countries eur
27

YUS!
--}

addEU :: AllianceMap -> FilePath -> IO AllianceMap
addEU = adder parseEU

adder :: (FilePath -> IO Alliance) -> AllianceMap -> FilePath -> IO AllianceMap
adder f ma file = uncurry (inserter ma) . (A.name &&& id) <$> f file
      where inserter m k v = Map.insert k v m

{--
>>> todoPrep (dear ++ moderns)
>>> let ma = it
>>> addEU ma (euDir ++ eu)
>>> let newmap = it
>>> Map.lookup (T.pack "European Union") newmap 
Just (Alliance {name = "European Union", aliases = fromList [], 
                countries = fromList ["Austria","Belgium","Bulgaria",...})

TA-DAH!
--}

-- You'll note that the EU has wikidata Q-identifiers ... we may, at some
-- future time, which to update our graph (do you remember our graph?) with
-- these identifiers. ... but that's a story to tell for another day.
