{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D07.Exercise where

-- yesterday ...

import Y2020.M12.D03.Solution as FirstGo

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

import Data.Set (Set)

import Y2020.M12.D01.Solution   -- for triage-stuff

import Graph.Query
import Graph.JSON.Cypher  -- for matchSet, consider using this function
import Data.Relation      -- for Attribute-type

addAliasNames :: Endpoint -> CountryTriage -> IO String
addAliasNames url = undefined

{--
`addAliasNames` adds aliases to countries, filtering out both aliases and
countries that have unicode points in their names, and reports out what it is
filtering out.
--}
