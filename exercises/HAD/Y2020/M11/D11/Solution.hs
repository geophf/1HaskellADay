{-# LANGUAGE OverloadedStrings    #-}

module Y2020.M11.D11.Solution where 

-- Okay, now, finally, we have alliances of the world in one structure:

import Y2020.M11.D10.Solution (go)

-- so, convert that to graph-data and upload to our graph.

-- ... you do remember our graph of countries / continents / airbases, yes?

import Y2020.M10.D12.Solution             -- for Country
import Y2020.M10.D28.Solution hiding (Alliance, name, countries) -- for Name
import Y2020.M10.D30.Solution             -- for AllianceMap

import Data.Relation
import Graph.Query

import Control.Arrow ((&&&))

import qualified Data.Map as Map
import qualified Data.Set as Set

-- that means converting our AllianceMap into a set of Relation-values

data Member = MEMBER_OF
   deriving Show

data AllianceNode = Ally Name | Nation Country
   deriving Eq

type AllianceRelation = Relation AllianceNode Member AllianceNode

allianceGraph :: AllianceMap -> [AllianceRelation]
allianceGraph = concat . map alliance2rels . Map.elems

alliance2rels :: Alliance -> [AllianceRelation]
alliance2rels =
   map (uncurry toRel) . sequence . (name &&& Set.toList . countries)

toRel :: Name -> Country -> AllianceRelation
toRel n = Rel (Ally n) MEMBER_OF . Nation

-- which means these things have to be instanced for this:

instance Node AllianceNode where
   asNode (Ally n) = constr "Alliance" [("name", n)]
   asNode (Nation c) = constr "Country" [("name", c)]

instance Edge Member where
   asEdge = const "MEMBER_OF"

-- with these relations we SHOULD be able to upload these Alliances to the Graph

{--
>>> graphEndpoint
...
>>> let url = it
>>> go
...
>>> let am = it
>>> cyphIt url (allianceGraph am)

... and unicode :/

Neo.ClientError.Statement.SyntaxError\",\"message\":\"Invalid input '2': 
expected '\\\\', ''', '\\\"', 'b', 'f', 'n', 'r', 't', UTF16 or UTF32 (line 1, 
column 74 (offset: 73))\\n\\\"MERGE (a:Alliance { name: \\\"African Union\\\" })
MERGE (b:Country { name: \\\"C\\\\244te d'Ivoire (Ivory Coast)\\\" }) 
MERGE (a)-[rel:MEMBER_OF]->(b)\\\"\\n
                                   ^\"}]}"

le sigh.

STOOPID TEXT-TYPE I USED U SO THIS WOULDN'T HAPPEN BUT ARE YOU HELPING HERE?

NOOOOOOoOOooOoOooOOoOOoOOoOooOoooOOoOoOOoOoOoOoOoOOOOOOooOOOoOoO!

... and now we can start to ask some alliance-y questions of our airbases.

cha. rite. AFTER we clense the Nations of that pesky uni-cody, smh.
--}
