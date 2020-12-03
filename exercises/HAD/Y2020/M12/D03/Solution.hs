{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Y2020.M12.D03.Solution where

{--
Okay, so now we have brand-spanking-new countries! YAYA!

... but that was the easy part.

Aliases.

But `aliases` isn't really atomic. There are several, possibly even disjoin,
things we will be doing to move the alias, complete, and fold it into the
country entity in our graph database.

One thing we can do is to add a list-property to the country entity, 
identifying its aliases.

Today's Haskell problem: take the triaged information, and add all alias names
for a country to that country in the graph-store.
--}

import Data.Char (ord)

import Data.List (partition)

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Y2020.M12.D01.Solution   -- for triage-stuff

import Graph.Query
import Graph.JSON.Cypher  -- for matchSet, consider using this function
import Data.Relation      -- for Attribute-type and Node-class

{--
We want to set a new property, `aliases`, to be all the country's aliases.

For example:

match (c:Country { name: "United States of America"})
set c.aliases = ["United States", "USA"]
return c

represents aliases as a (Cypher) list. We don't need the return statement,
but, if present, it returns, e.g.:

{
  "identity": 226,
  "labels": [
    "Country"
  ],
  "properties": {
    "name": "United States of America",
    "qid": "http://www.wikidata.org/entity/Q30",
    "aliases": [
      "United States",
      "USA"
    ]
  }
}

This is what we're doing today.
--}

addAliasNames :: Endpoint -> CountryTriage -> IO String
addAliasNames url =
   getGraphResponse url
   . map (matchSet "c" . fst <*> aliasesFor . snd)
   . Map.toList
   . linked

-- a helper function to generate the relation for matchSet:

aliasesFor :: Set OrphanedCountry -> Attribute [String]
aliasesFor = ("aliases",) . map name . Set.toList

{--
e.g.:

>>> aliasesFor usaSet
("aliases", ["USA", "United States"])
--}

-- and to stitch this all together, Country needs to be Nodeifiable

instance Node Country where
   asNode (Country c) = constr "Country" [("name", c)]

-- How many Countries were updated with aliases?

{--
>>> triageCountries 
Triage {linked = fromList [(Country {country = "Bangladesh"},...)]}
>>> let triage = it

>>> graphEndpoint 
>>> let url = it
>>> addAliasNames url triage
... errors ...

because ... unicode translations. le. sigh.
--}

{-- BONUS -------------------------------------------------------

Report out which aliases we don't do due (three 'do'-sounding words in a row)
to unicode point-issues.
--}

aliasesForM :: Country -> Set OrphanedCountry -> IO (Attribute [String])
aliasesForM c = filterAndReport c . aliasesFor 

filterAndReport :: Country -> Attribute [String] -> IO (Attribute [String])
filterAndReport c alias@(n, aliases) =
   let (goods, bads) = partition (all ((< 128) . ord)) aliases
   in  putStrLn ("For " ++ country c ++ ":") >>
       putStrLn ("\tadding: " ++ show goods) >>
       putStrLn ("\tskipping " ++ show bads) >>
       return (n, goods)

addAliasesNamesM :: Endpoint -> CountryTriage -> IO String

-- note: this is the exact same type-signature as addAliasesNames.
-- ... aren't monadic domains grand?

addAliasesNamesM url triage =
   mapM (uncurry aanm') (Map.toList (linked triage)) >>=
   getGraphResponse url

aanm' :: Country -> Set OrphanedCountry -> IO Cypher
aanm' c alis = aliasesForM c alis >>= return . matchSet "c" c

{--
>>> addAliasesNamesM url triage
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

... BUT THEN!

{"results":[...],
... "errors":[{"code":"Neo.ClientError.Statement.SyntaxError",
 "message":"Invalid input ...
  "MATCH (c:Country { name: \"S\\227o Tom\\233 and Pr\\237ncipe\" }) SET ..."

So, not only can the aliases be unicode-y, but so can the source countries!

But let's leave that work for tomorrow's exercise.
--}
