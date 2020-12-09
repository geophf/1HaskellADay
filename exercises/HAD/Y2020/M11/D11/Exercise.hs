{-# LANGUAGE OverloadedStrings    #-}

module Y2020.M11.D11.Exercise where 

-- Okay, now, finally, we have alliances of the world in one structure:

import Y2020.M11.D10.Exercise (go)

-- so, convert that to graph-data and upload to our graph.

-- ... you do remember our graph of countries / continents / airbases, yes?

import Y2020.M10.D12.Exercise             -- for Country
import Y2020.M10.D28.Exercise hiding (Alliance, name, countries) -- for Name
import Y2020.M10.D30.Exercise             -- for AllianceMap

import Data.Relation
import Graph.Query

-- that means converting our AllianceMap into a set of Relation-values

data Member = MEMBER_OF
   deriving Show

data AllianceNode = Ally Name | Nation Country
   deriving Eq

allianceGraph :: AllianceMap -> [Relation AllianceNode Member AllianceNode]
allianceGraph = undefined

-- which means these things have to be instanced for this:

instance Node AllianceNode where
   asNode = undefined

instance Edge Member where
   asEdge = undefined

-- with these relations we should be able to upload these Alliances to the Graph

{--
>>> graphEndpoint
...
>>> let url = it
>>> go
...
>>> let am = it
>>> cyphIt url (allianceGraph am)

... and now we can start to ask some alliance-y questions of our airbases.
--}
