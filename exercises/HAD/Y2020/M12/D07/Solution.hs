{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D07.Solution where

-- yesterday ...

import qualified Y2020.M12.D03.Solution as FirstGo

{--
... we tried to add aliases to countries in our graph store. And, because there
were unicode points in the aliases, it didn't work.

BONUS was to filter out those aliased.

But did it work then?

No, because some Countries have unicode points, too.

Now, what we HAVE been doing is filtering out the names that aren't 'unicode-
compliant' ... but are they? Or are they not?

neo4j is saying one thing: "I'm expecting this representation for a unicode
point." And Haskell is sending a different representation. Is it the correct
one? Do we need to change Data.Aeson to correct the representation? Or do we
need to correct neo4j's REST endpoint to accept the correctly-formatted JSON
with respect to unicode points?

The latter seems to be out of our control...*

* neo4j is an open-source project on git. You can fork it and make your own
... idk: neo5j, if you'd like.

Just some thoughts here on unicode-point representation in JSON.

Now, setting those thoughts aside.

Yesterday, we filtered out the aliases that had unicode points, but we failed
to filter out countries stored with unicode points in their names.

Today, let's do both.
--}

import Data.Char (ord)

import Data.List (partition)

import qualified Data.Map as Map

import Data.Set (Set)

import Y2020.M12.D01.Solution   -- for triage-stuff

import Graph.Query
import Graph.JSON.Cypher  -- for matchSet, consider using this function
import Data.Relation      -- for Attribute-type

addAliasNames :: Endpoint -> CountryTriage -> IO String
addAliasNames url triage =
   mapM (uncurry aanm'') (Map.toList (linked triage)) >>=
   getGraphResponse url . concat

{--
`addAliasNames` adds aliases to countries, filtering out both aliases and
countries that have unicode points in their names, and reports out what it's
filtering out.
--}

aanm'' :: Country -> Set OrphanedCountry -> IO [Cypher]
aanm'' c alis = maybe [] (return . matchSet "c" c) <$> aliasesForM' c alis

type MbAttrIO = IO (Maybe (Attribute [String]))

aliasesForM' :: Country -> Set OrphanedCountry -> MbAttrIO
aliasesForM' c = filterAndReport' c . FirstGo.aliasesFor 

filterAndReport' :: Country -> Attribute [String] -> MbAttrIO
filterAndReport' (Country c) = forCountry c (any ((> 127) . ord) c)

forCountry :: String -> Bool -> Attribute [String] -> MbAttrIO
forCountry country True (_, aliases) =
   putStrLn ("Country " ++ country
             ++ " has unicode points. Skipping " ++ show aliases) >>
   return Nothing
forCountry country False (n, aliases) =
   putStrLn ("For " ++ country ++ ":") >>
   let (goods, bads) = partition (all ((< 128) . ord)) aliases
   in  putStrLn ("\tadding: " ++ show goods) >>
       putStrLn ("\tskipping " ++ show bads) >>
       return (Just (n, goods))

{--
>>> triageCountries 
Triage {linked = fromList [(Country {country = "Bangladesh"},...)]}
>>> let triage = it
>>> graphEndpoint 
>>> let url = it
>>> addAliasNames url triage
For Bangladesh:
	adding: ["Bangladesh1"]
	skipping []
For Cambodia:
	adding: ["Cambodia1"]
	skipping []
For Cape Verde:
	adding: ["Cabo Verde"]
	skipping []
For Chad:
	adding: ["Tchad"]
	skipping []
For China:
	adding: ["People's Republic of China"]
	skipping []
For Cyprus:
	adding: []
	skipping ["\922\973\960\961\959\962"]   -- Κύπρος, done
For Democratic Republic of the Congo:
	adding: ["Zaire"]
	skipping []
For East Timor:
	adding: ["Timor Leste"]
	skipping []
For Egypt:
	adding: ["Misr"]
	skipping []
For Ethiopia:
	adding: ["Abyssinia"]
	skipping []
For Iceland:
	adding: []
	skipping ["\205sland"]    -- done
For Iraq:
	adding: ["Iraqi"]
	skipping []
For Montenegro:
	adding: []
	skipping ["\1062\1088\1085\1072 \1043\1086\1088\1072"] -- Црна Гора, done
For Morocco:
	adding: ["Al Maghrib"]
	skipping []
For North Macedonia:
	adding: []
	skipping ["\1052\1072\1082\1077\1076\1086\1085\1080\1112\1072"] -- Македонија
For Republic of Ireland:
	adding: ["Ireland"]
	skipping []
For Saint Kitts and Nevis:
	adding: ["St. Kitts and Nevis"]
	skipping []
For Saint Lucia:
	adding: ["St. Lucia"]
	skipping []
For Saint Vincent and the Grenadines:
	adding: ["St. Vincent and the Grenadines"]
	skipping []
For Slovenia:
	adding: ["Slovenija"]
	skipping []
Country São Tomé and Príncipe has unicode points. Skipping ["Sao Tome and Principe"] -- done
For The Bahamas:
	adding: ["Bahamas"]
	skipping []
For United Arab Emirates:
	adding: ["UAE"]
	skipping []
For United Kingdom:
	adding: ["UK"]
	skipping []
For United States of America:
	adding: ["USA","United States"]
	skipping []
{"results":[{"columns":[],"data":[]},...,"errors":[]}

WOOT!

Now add the skipped values manually to your graph-store, like so:

match (c:Country { name: "São Tomé and Príncipe"})
set c.aliases = ["Sao Tome and Principe"]
return c
--}
