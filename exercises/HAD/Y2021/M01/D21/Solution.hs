{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Y2021.M01.D21.Solution where

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

import Control.Arrow ((&&&))

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

{--
>>> readWineries (wineriesDir ++ wineriesJSON)
...
>>> let wikiwineries = it
>>> Map.size wikiwineries 
610
--}

type Idx = Integer
type IxWineries = Map Name Idx

wineriesQuery :: Cypher
wineriesQuery = "MATCH (w:Winery) RETURN w.name AS winery, id(w) AS ix"

wineriesFromGraph :: Endpoint -> IO IxWineries
wineriesFromGraph url =
   Map.fromList . mapMaybe (list2tup . RR.row) . RR.justRows
       <$> getGraphResponse url [wineriesQuery]

list2tup :: [Value] -> Maybe (Name, Idx)
-- list2tup = sequence . (fromJSON1 . head &&& fromJSON1 . last) -- ugh
list2tup [a,b] = fromJSON1 a >>= \ma -> fromJSON1 b >>= return . (ma,)

fromJSON1 :: FromJSON a => Value -> Maybe a
fromJSON1 = reifySuccess . fromJSON

reifySuccess :: Result a -> Maybe a
reifySuccess (Success a) = Just a
reifySuccess _           = Nothing

{--
>>> graphEndpoint 
...
>>> let url = it
>>> wineriesFromGraph url
fromList [("1+1=3",8695),("10 Knots",6989),("100 Percent Wine",10397),...]

How many wineries are in the graph database?

>>> let graphWineries = it
>>> Map.size graphWineries 
16757

Okay, so there's quite a disparity between the two data sets.
--}

wineries2wineries :: Wineries -> IxWineries -> Set Name
wineries2wineries w = Map.keysSet . Map.intersection w

-- How many wineries are in the intersection?

{--
>>> let geowineries = wineries2wineries wikiwineries graphWineries 
>>> Set.size geowineries 
125
>>> geowineries 
fromList ["Anglim","Ar\237nzano","Au Bon Climat","Beaulieu Vineyard",
          "Belden Barns","Bodega de Edgar","Bodegas Ateca","Bodegas Paso Robles",
          "Bosman Family Vineyards","Brecon Estate","Brochelle Vineyards",
          "Brown Estate","Casaj\250s","Castello di Amorosa","Chateau Chantal",
          "Chateau Grand Traverse","Chateau Montelena","Chateau St. Jean",
          "Chateau Ste. Michelle","Ch\226teau Ang\233lus",...]
--}

{-- BONUS -------------------------------------------------------

Are there wineries that should intersect, but don't? Which ones? How would
you fix this non-intersection (if it exists) to be in the intersection-set?

... actually, the winery's country helps quite a bit to answer this bonus,
so we will defer it until tomorrow.
--}

-- hmmm, shall we upload LongLat data to the graph? YES! ... but not today.
