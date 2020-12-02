{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D02.Exercise where

-- so, yesterday:

import Y2020.M12.D01.Solution

{--
We were able to triage our countries. Today we are going to use the triage
to start correcting the orphaned countries ... the ones we care about, anyway.

Sorry, Yugoslavia. We miss the Yugo, too.

Okay, let's create those 'new' countries that we missed in previous passes,
by associating them to their ... well: associated continents.
--}

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher (Cypher)
import Graph.JSON.Cypher.Read.Rows

import Data.Text (Text)
import qualified Data.Text as T

createNewCountries :: CountryTriage -> Endpoint -> IO String
createNewCountries = undefined

{-- 
Use `triageCountries` to get the triage. Which new countries did you create?

Okay, but I say: `createNewCountries`. But how does one go about doing that?

We create the Relation values that represent a (new) country being `IN`
the continent. Then we `cyphIt` those relations.
--}

data IN = IN
   deriving (Eq, Show)

instance Edge IN where
   asEdge = undefined

instance Node OrphanedCountry where
   asNode = undefined

instance Node Continent where
   asNode = undefined

{-- BONUS -------------------------------------------------------

There was one country in the `others` part of the triage that is just all
unicode points. What country is that? How do we find that out with Haskell's
representation of unicode-in-strings?

One approach is to see what this country relates to, then trace it down, this
way.

My hypothesis is that this country is an alias. If verified, to which country
should this `others` country alias?
--}

relationsToOthers :: Endpoint -> CountryTriage -> IO String
relationsToOthers = undefined

-- to get the data out of the graph-store, here's a query to do that:

queryCountries :: Text -> Cypher
queryCountries name =
   T.concat ["match p=(c:Country { name: '", name, "' })--() return p"]
