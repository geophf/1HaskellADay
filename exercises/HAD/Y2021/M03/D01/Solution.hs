{-# LANGUAGE OverloadedStrings #-}

module Y2021.M03.D01.Solution where

{--
Okay, we have some matches of wineries between the wikidata-set and our
graph-set using the double-metaphone algorithm. Great! Now, let's actually 
match those matches, that is to say: upload the matching wikidata data to our 
graph-store.

Several steps to complete this exercise.

0. We have to somehow indicate that a match is good with an 'approved'-marker.
1. We have the names of the wikidata wineries, we need to match those names back
   to the full data set (QNames and geolocations).
2. We have to, then, upload those data to the matched graph-wineries.
3. We have to indicate with an aliased winery-node, that these data are
   matched.

Let's do it.

1. match back. Match the wikidata winery-name back to the wikidatum.
--}

import Control.Arrow ((***))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import qualified Data.Set as Set

import qualified Data.Text as T

import Data.Tuple (swap)

import Graph.Query

import Data.Aeson.WikiDatum

import Y2021.M01.D21.Solution (Idx)
import Y2021.M01.D22.Solution                   -- for wineries
import Y2021.M01.D29.Solution                   -- Namei
import Y2021.M02.D26.Solution                   -- for metaphone-matches
import Y2021.M02.D23.Solution (todaysDir)
import Y2021.M02.D25.Solution                   -- for IxWinery

-- which, of course, means load the wikidata and the matched set.

{--
>>> readWineries (wineriesDir ++ wineriesJSON)
fromList [...]
>>> let wikiws = it

>>> loadWikiWineries (todaysDir ++ "wiki-wineries-metaphones.json")
fromList [...]
>>> let wmf = it
>>> graphEndpoint >>= fetchGraphWineries 
fromList [...]
>>> let gmf = it
>>> let mws = matchWineries gmf wmf

Okay, so we have the wineries and the matched metaphone set.
--}

type ApprovedAliases = Map Name IxWinery

-- ApprovedAliases matches the wikiwinery name to the graph-(indexed)-winery

-- There are various ways to get what we want approved into our approval-list.
-- I'm going to go with the functional approach

approve :: (Name, IxWinery) -> Bool
approve ("Ch\226teau Golan", WN "Ch\\226teau Calon" _)       = True
approve ("Ch\226teau La Coste", WN "Ch\\226teau la Coste" _) = True
approve ("Recaredo", WN "Recuerdo" _) = True
approve ("Ch\226teau de Goulaine", WN n _) = 
   "Ch\\226teau de Go\\235lane" `T.isPrefixOf` n
approve (w, _) =
   let chateaux = Set.fromList ["Haute Cabriere", "Ch\226teau-Figeac", 
                "Ch\226teau de Marsannay", "Ch\226teau Doisy Da\235ne"]
       prefices = ["Ch\226teau Grand-Puy-Ducas",
                   "Ch\226teau L\233oville-Las",
                   "Ch\226teau La Gaffeli\232r",
                   "Ch\226teau La Tour Blanche",
                   "Ch\226teau Sainte-Roseline",
                   "Ch\226teau Ducru-Beaucaill",
                   "Ch\226teau de Rayne-Vignea",
                   "Ch\226teau de l'Oiselini"]
   in  Set.member w chateaux || any (`T.isPrefixOf` w) prefices

-- transform the matched wineries Map M' (Set IxWinery) (Set IxWinery)
-- to (Name, IxWinery)-pairs and map them to approve

-- How many elements are in the ApprovedAliases-map?

instance Namei IxWinery where
   namei (WN n _) = n

map2FlatList :: MatchedWineries -> [(Name, IxWinery)]
map2FlatList =
   concat
   . map (\(f,s) ->[(namei n, g) | n <- Set.toList f, g <- Set.toList s])
   . Map.elems

vetMatchedWineries :: MatchedWineries -> ApprovedAliases
vetMatchedWineries = Map.fromList . filter approve . map2FlatList

-- finally replace the wikidata winery names with the wikidata wineries

type AliasedWineries = Map Idx Winery

instance Indexed IxWinery where
   ix (WN _ i) = i

names2wineries :: Wineries -> ApprovedAliases -> AliasedWineries
names2wineries ws =
   Map.fromList
   . mapMaybe (sequence . (ix *** n2w ws) . swap)
   . Map.toList

-- this helper function may be useful

n2w :: Wineries -> Name -> Maybe Winery
n2w = flip Map.lookup

-- That's today's problem. Tomorrow we'll upload the matches (with their 
-- aliases) to the graph-store.

-- Here is a handy function that gives you the approved aliases

aliasedWineries :: IO AliasedWineries    -- making file-place assumptions
aliasedWineries =
   readWineries (wineriesDir ++ wineriesJSON)                      >>= \wikiw ->
   loadWikiWineries (todaysDir ++ "wiki-wineries-metaphones.json") >>= \wmf ->
   graphEndpoint >>= fetchGraphWineries                            >>= \gmf ->
   return (names2wineries wikiw (vetMatchedWineries (matchWineries gmf wmf)))

{--
>>> aliasedWineries
fromList [...]
>>> let alis = it
>>> length alis
16

>>> head $ Map.toList alis
(2805,Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q67201150",...}})
--}
