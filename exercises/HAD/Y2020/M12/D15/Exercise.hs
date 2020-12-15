{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D15.Exercise where

{--
If you recall from yesterday's exercise, we saw, in the bonus, that there
were quite a few countries that did not have associated capitals. Since I
mapped alliances and their countries, anchored by the capitals, that makes
the mapping exercise unworkable for "some" (many) alliances, even after we
did the data-correction for countries' aliases.

What to do?

Welp, go back to wikidata with a better query:

SELECT ?country ?countryLabel ?capital ?capitalLabel ?latlongLabel 
WHERE 
{
  ?country wdt:P31 wd:Q6256.
  ?country wdt:P36 ?capital.
  ?country wdt:P625 ?latlong.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}

You see this query differs from the previous attempt at:
--}

import Y2020.M11.D17.Solution (CountryInfoMap, CapAt)
import qualified Y2020.M11.D17.Solution as Caps

import Y2020.M10.D30.Solution hiding (name)     -- for Alliance

import Y2020.M12.D01.Solution (Country, mkCountry)
import Y2020.M12.D03.Solution               -- for Country-Node instance
import qualified Y2020.M12.D10.Solution as Missn

import Data.Aeson.WikiDatum (Name, WikiDatum, LongLat)
import qualified Data.Aeson.WikiDatum as WD

import Data.Relation

import Graph.Query (graphEndpoint, cyphIt, Endpoint)
import Graph.JSON.Cypher

import Control.Arrow ((***))
import Control.Monad (join)

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

newCapsDir :: FilePath
newCapsDir = "Y2020/M12/D15/"

newCapsJSON :: FilePath
newCapsJSON = "capitals.json"

{--
In that it does not depend on the type of `capital` but on the type of
`country` having a `capital` property. This query is better how?

Well, I get better results: more countries with capitals that I can use
to do our next data correction.

>>> Caps.readCapitals (newCapsDir ++ newCapsJSON)
...
>>> let caps = it
>>> Map.size it
168
>>> :set -XOverloadedStrings 
>>> Map.lookup "Brazil" caps
Just (CI {country = WD {qid = "http://www.wikidata.org/entity/Q155", 
                       name = "Brazil"},
          capital = WD {qid = "http://www.wikidata.org/entity/Q2844", 
                        name = "Bras\237lia"},
          latLong = point({ latitude: -14.0, longitude: -53.0 })})
>>> Map.lookup "Singapore" caps
Just (CI {country = WD {qid = "http://www.wikidata.org/entity/Q334", 
                        name = "Singapore"}, 
          capital = WD {qid = "http://www.wikidata.org/entity/Q334", 
                        name = "Singapore"}, 
          latLong = point({ latitude: 1.3, longitude: 103.8 })})

Okay!

Now let's snarf the capitaless countries from the graph data-store using
yesterday's bonus-approach:

>>> graphEndpoint 
...
>>> let url = it
>>> Missn.capitalless url Missn.noCAPSquery
fromList ["Albania","Angola","Antigua and Barbuda","Armenia","Austria",...]
>>> let missins = it
>>> Set.size missins 
87

Okay. We have our work cut out for us.

So, first:

How many capitalless-countries, in the graph database, have capitals covered
by the new wikidata set here?
--}

missinFindin :: CountryInfoMap -> Set Name -> (Set Name, Set Name)
missinFindin = undefined

-- partitions countries into ones without capitals and ones with

{--
>>> let ans@(stillMissins, foundins) = missinFindin caps missins
>>> join (***) Set.size ans
(21,66)

But this is deceptive because:

>>> Set.filter (T.isInfixOf "Irel") stillMissins 
fromList ["Republic of Ireland"]

and:

>>> Set.filter (T.isInfixOf "Irel") (Map.keysSet caps)
fromList ["Ireland"]

So we have to address aliasing. But not right now.

Importantly, for the Five Power Defense Arrangement, Singapore's capital is
present:

>>> Set.filter (T.isInfixOf "Sing") foundins 
fromList ["Singapore"]

So, let's address the 66 countries in findins that we do have capitals for,
directly. Now, we have a capital-type in Y2020.M11.D17.Solution.CC2, but I don't
like it, as I forgot, then, to upload the lats/longs with capitals.

Let's recitify that now.
--}

data Capital = Capital WikiDatum LongLat
   deriving (Eq, Ord, Show)

instance Node Capital where
   asNode = undefined

{-- 
With that Capital-declaration, above, and the CapAt and Capital-types
(imported). We should be able to build the relations that add capitals
to `foundins`.
--}

type CountryCapRel = Relation Country CapAt Capital

foundCountryCapitals :: CountryInfoMap -> Set Name -> Set CountryCapRel
foundCountryCapitals = undefined

{--
>>> let ccrels = foundCountryCapitals cim foundins
>>> Set.size ccrels
66

>>> head $ Set.toList ccrels
Rel (Country {country = "Antigua and Barbuda"}) 
    CAPITAL 
    (Capital (WD {qid = "http://www.wikidata.org/entity/Q36262", 
                  name = "St. John's"}) 
             point({ latitude: 17.116666666, longitude: -61.85 }))

... let's upload these new relations to the graph store.

>>> cyphIt url ccrels

but ...
 
... Neo.ClientError.Statement.SyntaxError:  name: \\\"Bras\\\\237lia\\\"

Okay, let's filter out unicode-pointed capitals, ... and suchlike:
--}

asciiOnly :: CountryCapRel -> Bool
asciiOnly = undefined

{--
>>> let (doThese, notThese) = Set.partition asciiOnly ccrels
>>> join (***) Set.size (doThese, notThese)
(60,6)

>>> cyphIt url doThese
{"results":[{"columns":[],"data":[]},...,,"errors":[]}

Nice.

And the Cypher to loaded the `notThese` is nearly written:

>>> putStrLn (T.unpack $ mkCypher "coun" "c" "cap" (head $ Set.toList notThese))
MERGE (coun:Country { name: "Brazil" }) 
MERGE (cap:Capital { name: "Bras\237lia",
                     qid: "http://www.wikidata.org/entity/Q2844",
                     location: point({ latitude: -14.0, longitude: -53.0 }) }) 
MERGE (coun)-[c:CAPITAL]->(cap)

So we just need the country, the capital, its qid and lat/long:
--}

printNotThese :: Set CountryCapRel -> IO ()
printNotThese = undefined

{--
>>> printNotThese notThese
["Brazil","http://www.wikidata.org/entity/Q2844","Bras\237lia",point({ latitude: -14.0, longitude: -53.0 })]
["Cameroon","http://www.wikidata.org/entity/Q3808","Yaound\233",point({ latitude: 7.0, longitude: 12.0 })]
["Costa Rica","http://www.wikidata.org/entity/Q3070","San Jos\233",point({ latitude: 10.0, longitude: -84.0 })]
["Maldives","http://www.wikidata.org/entity/Q9347","Mal\233",point({ latitude: 4.18, longitude: 73.51 })]
["Paraguay","http://www.wikidata.org/entity/Q2933","Asunci\243n",point({ latitude: -23.5, longitude: -58.0 })]      
["Togo","http://www.wikidata.org/entity/Q3792","Lom\233",point({ latitude: 8.25, longitude: 1.183333 })]
--}

{-- BONUS -------------------------------------------------------

Now that all the countries of the Five Powers Defense Arrangements have
capitals with (importantly) lats and longs, map this alliance, its countries,
and their air bases onto a global-viewer using KML/Keyhole Markup Language.
--}

fivePowers :: Endpoint -> Alliance -> IO String
fivePowers = undefined

-- ... you have to fetch that alliance, ... somehow, and the associated data.

-- look to, e.g.: Y2020.M11.D23.Solution.nato for an approach for this
