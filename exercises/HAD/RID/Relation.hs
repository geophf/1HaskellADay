{-# LANGUAGE OverloadedStrings #-}

module RID.Relation where

{-- a solution to the problem posted at http://lpaste.net/3230687675795111936
and then fully fleshed-out at http://lpaste.net/2598821222603030528

Okay, so we looked at the RID as a Tree, then as a Data.Graph, then as JSON.

Now let's look at the RID as a set of relations, another, generic, way of 
looking at data.

From the RID (as a tree, above or the JSON or the plain-text) realize those
data as a set of relations in a relation-structure of your choosing.

JSON is at: http://lpaste.net/raw/5715293988441817088
Plain text: http://lpaste.net/raw/8106042088711258112
--}

import Control.Arrow
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Relation
import Graph.JSON.Cypher
import RID.Tree

rid2rels :: RID -> [Relation Kinds CatRel Kinds]
rid2rels = cogs                                     >>>
           map cognate . Map.keys                   &&&
           concatMap (uncurry cog2cat) . Map.toList >>>
           uncurry (++)

-- okay first we need a mapping of categories to categories, then we need
-- the mapping of end(o)-categories to the words in them.

-- Who walks around saying endocategories? Or endofunctors in the monoid
-- category when they simply mean (real) functions? Who? Huh? Who?

data CatRel = COGNITION | SUBCATEGORY | WORDIN | CATEGORY
   deriving (Eq, Ord, Enum, Show)
instance Edge CatRel where asEdge = show

-- notice a neat thing about the semantics of these relations: they are declared
-- from the words 'up-to' their root categories.

cognate :: Cognition -> Relation Kinds CatRel Kinds
cognate = Rel Root COGNITION . Cog

cog2cat :: Cognition -> Map String Category -> [Relation Kinds CatRel Kinds]
cog2cat cog = map (Rel (Cog cog) CATEGORY . Catg) . Map.keys &&&
              concatMap mapCat . Map.elems                   >>>
              uncurry (++)

{--
   map (\(Rel a rel c) -> Rel a rel (C' c)) (concatMap mapCat (Map.elems cats))
   ++ map (\cat -> Rel (C $ Kat cat) CATEGORY (R' Primary)) (Map.keys cats)
--}

{-- and then we map the categories to the primary process cognition root

data Class = C' CatName | R' Root deriving (Eq, Show)

data Root = Primary deriving (Eq, Show)

instance Node Kinds where
   asNode (Catg cat) = asNode cat
   asNode (Cog cog) = "ROOT { cognition: '" ++ show cog ++ "' }"
--}

mapCat :: Category -> [Relation Kinds CatRel Kinds]

-- so here how's this works: a subcategory mapping creates a set of relations
-- mapping from the category name to each subcategory (n relations), THEN it
-- maps each subcategory name to each of ITS subcategories until we exhaust
-- categories.

-- Then at the end(o)-categories it maps each subcategory name to each of its
-- root word value.

-- And then we're done.

-- The category's type is CATEGORY and the RootWord type is ROOTWORD, in case
-- you were wondering

mapCat w@(WordSet cat wrdsmap) = mapCatsOrWords cat w
mapCat c@(Cat cat cats)        = mapCatsOrWords cat c

mapCatsOrWords :: String -> Category -> [Relation Kinds CatRel Kinds]
mapCatsOrWords cat (WordSet _ wrds) =
   map (Rel (Catg cat) WORDIN . Rwrd) (Map.elems wrds)

mapCatsOrWords cat (Cat catname cats) = 
   map (Rel (Catg catname) SUBCATEGORY . Catg) (Map.keys cats)

-- we got the category names, now we need the categories of the categories!

   ++ concatMap mapCat (Map.elems cats)

{--
mapping :: Kinds -> CatRel -> Kinds -> Relation Kinds CatRel CatName
mapping parent rel kid = Rel kid rel parent

-- data Child = RW RootWord | C CatName deriving (Eq, Show)

data CatName = Kat String deriving (Eq, Ord)

instance Show CatName where show (Kat s) = s
instance Node CatName where
   asNode (Kat s) = "CATEGORY { name: '" ++ s ++ "' }"
--}

instance Node Kinds where
   asNode Root     = "RID { name: 'RID', aka: 'Regressive Imagery Dictionary' }"
   asNode (Cog c)  = "ROOT { cognition: '" ++ show c ++ "' }"
   asNode (Catg s) = "CATEGORY { name: '" ++ s ++ "' }"
   asNode (Rwrd w) = asNode w

instance Node RootWord where
   asNode (RW wrd star) =
      "MATCHWORD { root: '" ++ wrd ++ "', hasWildcard: '" ++ has star ++ "' }"
          where has x = if x then "YES" else "NO"

-- what are the types a, rel, and b in your answer?
-- answers a = Child, rel = CatRel, b = CatName

{-- How many relations did you realize from the RID?
*RID.Relation> readinRID "RID" ~> rid
*Main> let rels = rid2rels rid
*Main> length rels ~> 3198
So, yeah.

... now it gets interesting ...
--}

{-- BONUS ------------------------------------------------------------------

Relations can be sent to a neo4j web service and represented as a neo4j-graph.

Do that. Show your resulting graph.
--}

rid2cypher :: RID -> [Cypher]
rid2cypher = map (mkCypher "root" "rel" "kid") . rid2rels

-- no biggy, but then we've got to formulate JSON from that.
-- let's save that to a file

saveRID :: FilePath -> RID -> IO ()
saveRID file = saveJSON file . rid2cypher

{--
*Main> saveRID "RID/rid.cyph" rid ~>
{"statements":[
   {"statement":"MERGE (kid:CATEGORY { name: 'CHAOS' }) MERGE (cat:CATEGORY { name: 'DEFENSIVE_SYMBOL' }) MERGE (kid)-[rel:SUBCATEGORY]->(cat)"},
   {"statement":"MERGE (kid:CATEGORY { name: 'DIFFUSION' }) MERGE (cat:CATEGORY { name: 'DEFENSIVE_SYMBOL' }) MERGE (kid)-[rel:SUBCATEGORY]->(cat)"},
   {"statement":"MERGE (kid:CATEGORY { name: 'PASSIVITY' }) MERGE (cat:CATEGORY { name: 'DEFENSIVE_SYMBOL' }) MERGE (kid)-[rel:SUBCATEGORY]->(cat)"},
   {"statement":"MERGE (kid:CATEGORY { name: 'RANDOM MOVEMENT' }) MERGE (cat:CATEGORY { name: 'DEFENSIVE_SYMBOL' }) MERGE (kid)-[rel:SUBCATEGORY]->(cat)"},
   {"statement":"MERGE (kid:CATEGORY { name: 'VOYAGE' }) MERGE (cat:CATEGORY { name: 'DEFENSIVE_SYMBOL' }) MERGE (kid)-[rel:SUBCATEGORY]->(cat)"},...

This cypher-JSON is saved out to http://lpaste.net/5542409679193243648
-- n.b. that link is PRIMARY cognition only ...

we cURL it into our neo4j system with:

geophf:RID geophf$ curl -u [redacted usr:passwd] -H "Content-Type: application/json" --data @rid.cyph http://localhost:7474/db/data/transaction/commit -X POST

and it responds with LOTS of JSON ending with ... ,"errors":[]} (no errors)

And in neo4j we see a nice graph of our primary cognition.

YAY!
--}
