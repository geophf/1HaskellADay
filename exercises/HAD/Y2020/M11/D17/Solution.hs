{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Y2020.M11.D17.Solution where

{--
So.

Okay.

I was going to go all: map countries and their airbases to a KML-file, but
to do that, I need a lat-long of the country, and where best to do that,
other than the country's capital. But we don't have country capitals, so
it's back to wikidata to get those data.

The PROBLEM with wikidata, see: is that it's wikidata, or messy as all get-out.

So, today, we're going to get these wikidata, then clean them up and transform
them to enhanced information on countries that we can use.

Here we go.

First, here is the query that gets us these data. There are some weirditudes.

# Countries and capitals
SELECT ?country ?countryLabel ?capital ?capitalLabel ?latlongLabel
WHERE 
{
  ?capital wdt:P31 wd:Q5119.
  ?capital wdt:P1376 ?country.
  ?capital wdt:P625 ?latlong.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}

n.b.: we do not constrain countries, because, if we do, we lose countries,
like Belgium. Let's sieve these data through the lens of the countries we
already have.
--}

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Aeson.WikiDatum

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher

import Y2020.M10.D12.Solution (AirBase, Icao, Country)  -- WAAAAAY back when for countries.
import qualified Y2020.M10.D12.Solution as A

data CountryInfo = CI { country :: WikiDatum, 
                        capital :: WikiDatum, 
                        latLong :: LongLat }
   deriving (Eq, Show)

instance FromJSON CountryInfo where
    parseJSON = withObject "CountryInfo" $ \v ->
       CI <$> v *: "country" <*> v *: "capital" <*> v @: "latlongLabel"

samp :: ByteString
samp = BL.concat ["{\"country\":\"http://www.wikidata.org/entity/Q31\"",
       ",\"countryLabel\":\"Belgium\",\"capital\":\"http://www.wikidata.org/",
       "entity/Q239\",\"capitalLabel\":\"Brussels\",\"latlongLabel\":\"Point",
       "(4.351666666 50.846666666)\"}"]

{--
>>> (decode samp) :: Maybe CountryInfo
Just (CI {country = WD {qid = "http://www.wikidata.org/entity/Q31", 
                        name = "Belgium"}, 
          capital = WD {qid = "http://www.wikidata.org/entity/Q239", 
                        name = "Brussels"},
          latLong = point({ latitude: 50.846666666, longitude: 4.351666666 })})

WOOT!

With this instance declaration, read in the file of ... "countries" and their
capitals.
--}

capitalDir :: FilePath
capitalDir = "Y2020/M11/D17/"

capitalsJSON :: FilePath
capitalsJSON = "things-capitals.json"

type CountryInfoMap = Map Country CountryInfo

readCapitals :: FilePath -> IO CountryInfoMap
readCapitals file = BL.readFile file >>=
   return . Map.fromList . maybe [] (map (name . country &&& id)) . decode

{--
>>> readCapitals (capitalDir ++ capitalsJSON)
>>> let caps = it
--}

-- How many country-infos thingies are there?

{--
>>> Map.size caps
767
--}

{--
2. Now, load the airbase map and `capitalize` it. But we don't want to do that.
What we want to do is filter out the `countries` that don't have airbases.
Let's do that, instead. 
   
>>> A.loadBases (A.workingDir ++ A.file)
>>> let bases = it
>>> let mappedBases = A.byICAO bases
--}    

countrySet :: Map Icao AirBase -> Set Country
countrySet = Set.fromList . map A.country . Map.elems
       
{--
>>> let cs = countrySet mappedBases 
>>> Set.size cs
108

Now we have a set of countries-that-are-countries by which we may identify
the things in the `CountryInfoMap` that are countries and not just things.
--}

{-- BONUS -------------------------------------------------------
                        
3. Upload Countries' Q-id's, their capitals (with their q-id's), and their
   lattness-longness to the graph data store. That means, of course, you must
   model these relations as ... well: ... relations.

--}

data CC1 = Countri WikiDatum
   deriving Eq

data CC2 = Capital WikiDatum LongLat 
   deriving Eq

instance Node CC1 where
   asNode (Countri wd) =
      constr "Country" [("name", name wd)]

-- ("qid", qid wd)] we leave off qid here, because we're going to add it
-- to the graph (see below)

instance Node CC2 where
   asNode (Capital wd ll) =
      constr "Capital" [("name", name wd), ("qid", qid wd)]

data CapAt = CAPITAL
   deriving (Eq, Show)

instance Edge CapAt where
   asEdge = T.pack . show 

type RelC1CaC2 = Relation CC1 CapAt CC2

countryCapitalCoordinates :: CountryInfo -> RelC1CaC2
countryCapitalCoordinates (CI cnt cap ll) =
   Rel (Countri cnt) CAPITAL (Capital cap ll)

-- with `countryCapitalCoordinates` you can cyph your data to the graph.

-- I recommend doing an update to countries to add their Q-id's first.

{--
Okay, adding a property to a node is thusness:

MATCH (n { name: 'Belgium' })   -- I'm Belgium-obsessed, but it's Tuesday.
SET n.qid = 'https://www.wikidata.org/wiki/Q31';

So, if we do:

>>> :set -XOverloadedStrings 
>>> let belgique = WD "http://www.wikidata.org/entity/Q31" "Belgium"
>>> matchSet "b" (Countri belgique) ("qid", qid belgique)
"MATCH (b:Country { name: \"Belgium\" }) b.qid=\"http://www.wikidata.org/entity/Q31\""

And we do that for every country in the set of countries.

cyphCountryInfo :: CountryInfoMap -> Set Country -> [Cypher]
cyphCountryInfo cim = mapMaybe (cci' cim) . Set.toList

cci' :: CountryInfoMap -> Country -> Maybe Cypher
cci' cim c = Map.lookup c cim >>=
             return . uncurry (matchSet "a")
                    . (Countri &&& ("qid",) . qid)
                    . country

-- then we do the same kind of thing for associating capitals to countries.

cyphCapitals :: CountryInfoMap -> Set Country -> [RelC1CaC2]
cyphCapitals cim = mapMaybe (cc' cim) . Set.toList

cc' :: CountryInfoMap -> Country -> Maybe RelC1CaC2
cc' cim c = countryCapitalCoordinates <$> Map.lookup c cim
--}

-- from which we can generalize to

cprimer :: (CountryInfo -> a) -> CountryInfoMap -> Country -> Maybe a
cprimer f cim c = f <$> Map.lookup c cim

-- and

citer :: (CountryInfo -> a) -> CountryInfoMap -> Set Country -> [a]
citer f cim = mapMaybe (cprimer f cim) . Set.toList

-- to yield:

cyphCountryInfo :: CountryInfoMap -> Set Country -> [Cypher]
cyphCountryInfo = citer cci

cci :: CountryInfo -> Cypher
cci = uncurry (matchSet "a") . (Countri &&& ("qid",) . qid) . country

-- and:

cyphCapitals :: CountryInfoMap -> Set Country -> [RelC1CaC2]
cyphCapitals = citer countryCapitalCoordinates
