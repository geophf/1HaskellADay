{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D03.Exercise where

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

import Data.Set (Set)

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
addAliasNames url triage = undefined

-- a helper function to generate the relation for matchSet:

aliasesFor :: Set OrphanedCountry -> Attribute [String]
aliasesFor aliases = undefined

{--
e.g.:

>>> aliasesFor usaSet
("aliases", ["USA", "United States"])
--}

-- and to stitch this all together, Country needs to be Nodeifiable

instance Node Country where
   asNode = undefined

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

aliasesForM :: Set OrphanedCountry -> IO (Attribute [String])
aliasesForM orphans = undefined

addAliasesNamesM :: Endpoint -> CountryTriage -> IO String

-- note: this is the exact same type-signature as addAliasesNames.
-- ... aren't monadic domains grand?

addAliasesNamesM = undefined
