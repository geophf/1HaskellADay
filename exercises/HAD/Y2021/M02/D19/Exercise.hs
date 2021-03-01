{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D19.Exercise where

{--
Okay, we have our ~120k wine-reviews uploaded, great! Did we do it correctly?

Hm.

Today's Haskell problem.

We've verified before (Y2021.M02.D04.Solution) that wine-labels and wineries 
weren't redundant, and, if they were, we eliminated those redundancies, by 
simply pouring all of the nodes of a type into a name-ids map. The duplicates 
find themselves.

For relations, it's a bit harder than that, and, that, on a couple of 
dimensions, for, a wine-reviewer can write more than one review of a particular
wine. Also, a relation is not its name (it has none), and its id is not useful,
either. The identifier of a relation is

(start-node-id, end-node-id)

but that is not unique (enough).

We also need the wine-review-text, itself, to check for redundancy.

AND we need the relation id to distinguish between the non-unique reviews.

Ugh. This is a big ball of work, but, so it goes.
--}

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Aeson (Value)

import Data.Text (Text)
import qualified Data.Text as T

import Graph.Query

import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D21.Solution (Idx)

import Control.Map (snarf)

type Vertices = (Idx, Idx)
type EdgeRels rel = Map Vertices (Set rel)

getWineRelationsQuery :: Cypher
getWineRelationsQuery =
   T.concat ["MATCH (t:Taster)-[r:RATES_WINE]->(w:Wine) ",
             "RETURN id(t) as taster_id, id(w) as wine_id, ",
             "id(r) as rel_id, r.review as review"]

data WineRel = WineRel { wrIx :: Idx, review :: Text }
   deriving (Eq, Ord, Show)

-- This function helps convert from a QueryResult to a (Vertices, WineRel) row

toPair :: [Value] -> Maybe (Vertices, WineRel)
toPair [a,b,c,d] = undefined

-- which means we can do this:

wineRels :: Endpoint -> IO (EdgeRels WineRel)
wineRels url = undefined

{--
>>> graphEndpoint >>= wineRels 
...
>>> let winrevs = it

How many wine-relations do you have from your graph store?
--}

-- Now that you have the relations, you must go into each set of Vertices
-- and find the duplicate relations.

dupeWinRels :: EdgeRels WineRel -> Set Idx
dupeWinRels winRels = undefined

-- What may be helpful is this function that finds dupes in a set of WinRels:

dupeWinRels' :: Set WineRel -> Set Idx
dupeWinRels' winRels = undefined

-- remember: 'dupe,' in this case, is: different idx but same review

-- How many dupes do we have?

{--
>>> let dupes = dupeWinRels winrevs 
--}

-- Okay, so, now we have ids of duplicate wine review relations.

-- Let's delete these duplicates (DUPLICATES, NOT ORIGINALS!)

deleteWineReviewsQuery :: Set Idx -> Cypher
deleteWineReviewsQuery ixs =
   T.pack (unwords ["MATCH (:Taster)-[r:RATES_WINE]->(:Wine) WHERE id(r) in", 
                    show (Set.toList ixs), "DELETE r"])

-- DOIT! TOIT!

{--
>>> graphEndpoint >>= flip getGraphResponse [deleteWineReviewsQuery dupes]
"{\"results\":[{\"columns\":[],\"data\":[]}],\"errors\":[]}"
--}