{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D21.Exercise where

import Y2021.M01.D20.Solution    -- for wineries

import Data.Aeson.WikiDatum (Name)

import Graph.Query
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

{--
Okay, yesterday, we got some lat/longs, or, more precisely, some LongLats
for wineries via wikipedia. Today, we're going to compare this against the
wine graph database.

To set up the wine graph database, upload the CSV files from the neo4j
repository, following the instructions of the README there:

https://github.com/lju-lazarevic/wine 

Now, that done, compare the wineries from the wikidata-set to the ones in the
wine graph database. What matches do we have?
--}

import Data.Map (Map)

import Data.Set (Set)

{--
>>> readWineries (wineriesDir ++ wineriesJSON)
...
>>> let wikiwineries = it
--}

type Idx = Integer
type IxWineries = Map Name Idx

wineriesQuery :: Cypher
wineriesQuery = "MATCH (w:Winery) RETURN w.name AS winery, id(w) AS ix"

wineriesFromGraph :: Endpoint -> IO IxWineries
wineriesFromGraph = undefined

-- How many wineries are in the graph database?

wineries2wineries :: Wineries -> Set Name -> Set Name
wineries2wineries = undefined

-- How many wineries are in the intersection?

{-- BONUS -------------------------------------------------------

Are there wineries that should intersect, but don't? Which ones? How would
you fix this non-intersection (if it exists) to be in the intersection-set?

... actually, the winery's country helps quite a bit to answer this bonus,
so we will defer it until tomorrow.
--}

-- hmmm, shall we upload LongLat data to the graph? YES! ... but not today.
