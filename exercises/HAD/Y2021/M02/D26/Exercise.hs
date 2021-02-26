{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D26.Exercise where

{--
Okay, we've uploaded metaphones to the graph-store. Now, today, let's match
metaphones from the cleaned-wikidata-set (remember cleaning the wikidata set?)
to the metaphones in the graph store.

How many 'good' matches do we have? How many matches do we get that we say:

"Eh? Really? That wiki-winery doesn't match the graph-winery at all!"?
--}

import Data.Aeson

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Text as T

import Graph.Query
import Graph.JSON.Cypher (Cypher)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D22.Solution                   -- for wineries

import Y2021.M02.D23.Solution (todaysDir)
import Y2021.M02.D25.Solution

-- FIRST! Load the wiki-wineries from JSON

loadWikiWineries :: FilePath -> IO WineriesMetaphones
loadWikiWineries jsonFileNanem = undefined

-- NOW! load the graph-wineries from the graph-store

metaphonesQuery :: Cypher
metaphonesQuery = 
   T.pack (unwords ["MATCH (m:Metaphone)-[r:METAPHONE]->(w:Winery)",
                    "RETURN m.primary, m.secondary, id(w), w.name"])

fetchGraphWineries :: Endpoint -> IO WineriesMetaphones
fetchGraphWineries url = undefined

-- which means we need to extract them from the returned graph

toPair :: [Value] -> Maybe (M', IxWinery)
toPair row = undefined

-- NOW! NOW! What are the wiki-wineries that match the graph-wineries using
-- metaphones?

type WineryMatches matcher = Map matcher (Set Winery, Set IxWinery)

matchWineries :: WineriesMetaphones -> WineriesMetaphones -> WineryMatches M'
matchWineries = undefined

-- how many matches did you get in total?
