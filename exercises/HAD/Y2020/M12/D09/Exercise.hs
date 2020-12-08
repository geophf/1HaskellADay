{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D09.Exercise where

{--
Okay! Yesterday we associated aliases to countries, YAY! Now, however, we
still have the problem that we have data (alliances) associated to the aliases,
not to the countries. We have to do that. Why?

So that the USA and UK can be part of NATO, so that our map looks pretty.

MAPS LOOKING PRETTY IS WHY WE ARE HERE, PEOPLE!

So.
 
1.a. To which alliances do these aliases belong; accumulate those.
1.b. associate those data to the source country entity.

2. Now, are there other things associated with these aliased countries?

2. answer: yes. Yes, there are: capitals.

So, today, let's do 1. above, and tomorrow we'll look at collecting capitals
for aliases.
--}

import Data.Aeson
import Data.Map (Map)
import Data.Set (Set)

import Data.Text (Text)
import qualified Data.Text as T

import Y2020.M10.D28.Solution hiding (alliance, Alliance) -- for Name
import Y2020.M11.D11.Solution                          -- Member-Edge instance
import Y2020.M12.D01.Solution (Country)
import Y2020.M12.D03.Solution               -- for Country-Node instance

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher

import Graph.JSON.Cypher.Read.Rows (TableRow)
import qualified Graph.JSON.Cypher.Read.Rows as RR

{--
We also need to filter out, again, countries and aliases that will break
queries to upload and to download data ... although downloading data won't
be a problem, but uploading a correction to those data will. So we need
to report that we don't do certain corrections... also what weirdo Text
values we're imp2orting.
--}

import qualified Y2020.M12.D07.Solution as UniFilterer

-- With that, let's query orphaned countries and their associated alliances.

data AliasAlliance = AA { alias :: Name, alliance :: Name }
   deriving (Eq, Ord, Show)

fetchAliasedAlliances :: Cypher
fetchAliasedAlliances =
   T.concat ["MATCH p=(c:Country)<-[:MEMBER_OF]-(d) ",
             "WHERE not (c)-[:IN]->(:Continent) ",
             "RETURN c.name, d.name"]

{--
Our query

>>> getGraphResponse url [fetchAliasedAlliances]

returns the JSON of `sampleAliasesAlliances`
--}

sampleAliasesAlliances :: String
sampleAliasesAlliances =
   concat ["{\"results\":[{\"columns\":[\"c.name\",\"d.name\"],\"data\":",
           "[{\"row\":[\"USA\",\"Moroccan-American Treaty of Friendship\"],",
           "\"meta\":[null,null]},{\"row\":[\"USA\",\"U.S.-Afghanistan Stra",
           "tegic Partnership Agreement\"],\"meta\":[null,null]}]}],",
           "\"errors\":[]}"]

{--
>>> (RR.justRows sampleAliasesAlliances) :: [TableRow [Text]]
[TR {row = ["USA","Moroccan-American Treaty of Friendship"]},
 TR {row = ["USA","U.S.-Afghanistan Strategic Partnership Agreement"]}]
--}

-- so, to get from the JSON result to AliasAlliance values we do

toAliasAlliance :: [Text] -> AliasAlliance
toAliasAlliance = undefined

{--
>>> let aas = map (toAliasAlliance . RR.row) (RR.justRows sampleAliasesAlliances)
>>> aas
[AA {alias = "USA", alliance = "Moroccan-American Treaty of Friendship"},
 AA {alias = "USA", alliance = "U.S.-Afghanistan Strategic Partnership Agreement"}]
--}

-- then we collect these result into a map:

type Alias = Name
type AliasAlliancesMap = Map Alias (Set Name)

aliasedAlliances :: [AliasAlliance] -> AliasAlliancesMap
aliasedAlliances = undefined

-- For an aliased country, collect and associate alliances to it in a map.

{--
>>> aliasedAlliances aas 
{("USA",{"Moroccan-American Treaty of Friendship",
         "U.S.-Afghanistan Strategic Partnership Agreement"})}

Okay! Now we need to replace the aliases with the source countries.

Or, that is to say: we need to lookup the source country from the alias:
--}

type AliasCountryMap = Map Alias Country

toAliasCountry :: [[Text]] -> AliasCountryMap
toAliasCountry = undefined

-- which we get from

aliasesCountriesQuery :: Cypher
aliasesCountriesQuery =
   T.concat ["MATCH (c:Country)<-[:MEMBER_OF]-(d:Alliance) ",
             "WHERE not (c)-[:IN]->(:Continent)  ",
             "WITH c.name as alias ",
             "MATCH (c:Country) WHERE alias in c.aliases ",
             "RETURN alias, c.name as country"]

{-- 
>>> getGraphResponse url [aliasesCountriesQuery]
"{\"results\":[{\"columns\":[\"alias\",\"country\"],\"data\":..."
>>> let aac = it
>>> let acm = toAliasCountry $ map RR.row (RR.justRows aac)
>>> acm
{("Abyssinia","Ethiopia"),("Al Maghrib","Morocco"),("Bahamas","The Bahamas"),
 ("Bangladesh1","Bangladesh"),("Cabo Verde","Cape Verde"),
 ("Cambodia1","Cambodia"),("Iraqi","Iraq"),("Ireland","Republic of Ireland"),
 ("Misr","Egypt"),("Sao Tome and Principe","S\227o Tom\233 and Pr\237ncipe"),
 ("Slovenija","Slovenia"),("St. Kitts and Nevis","Saint Kitts and Nevis"),
 ("St. Lucia","Saint Lucia"),
 ("St. Vincent and the Grenadines","Saint Vincent and the Grenadines"),
 ("Tchad","Chad"),("Timor Leste","East Timor"),("UAE","United Arab Emirates"),
 ("UK","United Kingdom"),("USA","United States of America"),
 ("United States","United States of America"),
 ("Zaire","Democratic Republic of the Congo"),("\205sland","Iceland"),
 ("\922\973\960\961\959\962","Cyprus"),
 ("\1062\1088\1085\1072 \1043\1086\1088\1072","Montenegro")]

Now we match the alliances to the source countries by replacing the alias
with the source country name, then we build a set of (new) relations from
each alliance to the source country.

Use the neo4j countries from Y2020.M12.D01.Solution and
Y2020.M12.D03.Solution for country-nodes, and Y2020.M11.D11.Solution for
the Member-type.
--}

data Alliance = Alliance Name
   deriving (Eq, Ord, Show)
   
instance Node Alliance where
   asNode = undefined

type CountryAllianceRel = Relation Alliance Member Country

mkCountryAlliances :: AliasCountryMap -> Alias -> Set Name -> [CountryAllianceRel]
mkCountryAlliances = undefined

-- from a row in the alias-alliance map, create a set of relations for uploading

{--
>>> let cas = concat . map (uncurry (mkCountryAlliances acm)) $ Map.toList aas
>>> cas
[Rel (Alliance "African Union") MEMBER_OF (Country {country = "Ethiopia"}),
 Rel (Alliance "African Union") MEMBER_OF (Country {country = "Morocco"}),...]

Now upload this to your graph data-store using cyphIt

>>> cyphIt url cas
"{\"results\":[{\"columns\":[],\"data\":[]},...
... "Neo.ClientError.Statement.SyntaxError\",\"message\":\"Invalid input...

Oops!

Tomorrow, we'll remove unicode-y relations (noting which ones we remove) and
delete the alliance MEMBER_OF relations to the aliases.
--}
