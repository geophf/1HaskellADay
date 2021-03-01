{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D26.Solution where

{--
Okay, we've uploaded metaphones to the graph-store. Now, today, let's match
metaphones from the cleaned-wikidata-set (remember cleaning the wikidata set?)
to the metaphones in the graph store.

How many 'good' matches do we have? How many matches do we get that we say:

"Eh? Really? That wiki-winery doesn't match the graph-winery at all!"?
--}

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Graph.Query
import Graph.JSON.Cypher (Cypher)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D22.Solution                   -- for wineries

import Y2021.M02.D23.Solution (todaysDir)
import Y2021.M02.D25.Solution

-- FIRST! Load the wiki-wineries from JSON

loadWikiWineries :: FilePath -> IO WineriesMetaphones
loadWikiWineries file = metaphones <$> fetchWineryMetaphones file

-- NOW! load the graph-wineries from the graph-store

metaphonesQuery :: Cypher
metaphonesQuery =
   T.pack (unwords ["MATCH (m:Metaphone)-[r:METAPHONE]->(w:Winery)",
                    "WHERE NOT((:AliasedWinery)-[:ALIAS_OF]->(w))",
                    "AND w.location IS NULL",
                    "RETURN m.primary, m.secondary, id(w), w.name"])

fetchGraphWineries :: Endpoint -> IO WineriesMetaphones
fetchGraphWineries url =
   RR.multimapBy toPair <$> getGraphResponse url [metaphonesQuery]

-- which means we need to extract them from the returned graph

toPair :: [Value] -> Maybe (M', IxWinery)
toPair [a,b,c,d] = RR.fromJSON1 a >>= \pri ->
                   RR.fromJSON1 b >>= \sec -> 
                   RR.fromJSON1 c >>= \idx ->
                   RR.fromJSON1 d >>= \nam ->
                   return (M' pri sec, WN nam idx)

{--
>>> loadWikiWineries (todaysDir ++ "wiki-wineries-metaphones.json")
fromList [...]
>>> let wikiws = it

>>> graphEndpoint >>= fetchGraphWineries 
fromList [...]
>>> let graphws = it
--}

-- NOW! NOW! What are the wiki-wineries that match the graph-wineries using
-- metaphones?

type MergedMaps k v1 v2 = Map k (v1, v2)
type MatchedWineries = MergedMaps M' (Set IxWinery) (Set IxWinery)

matchWineries :: WineriesMetaphones -> WineriesMetaphones -> MatchedWineries
matchWineries graphws =
   Map.fromList
   . mapMaybe (\(k,v) -> Map.lookup k graphws >>= \v1 -> return (k, (v, v1)))
   . Map.toList

-- so, matchWineries devolves into mergeMaps

-- how many matches did you get in total?

{--
>>> let mws = matchWineries graphws wikiws
>>> length mws
30

But:

>>> head (Map.toList mws)
(M' "ALST" "",(fromList [WN "Al Este" 0],fromList [WN "Alice White" 11859]))

No.

Let's look at the rest of them.
--}

report :: (Show k, Show v1, Show v2) => MergedMaps k (Set v1) (Set v2) -> IO ()
report = mapM_ printRow . Map.toList

printRow :: (Show k, Show v1, Show v2) => (k, (Set v1, Set v2)) -> IO ()
printRow (k, (sv1, sv2)) =
   putStrLn (show k ++ ":") >> printColumns (Set.toList sv1) (Set.toList sv2)

i :: String -> String
i = indent 5

printColumns :: (Show a, Show b) => [a] -> [b] -> IO ()
printColumns (a:as) (b:bs) =
   putStrLn (pad 35 (i $ show a) ++ take 35 (i $ show b)) >> printColumns as bs
printColumns [] bs@(_:_) = pc2 bs
printColumns (a:as) [] = pc1 as
printColumns [] [] = putStrLn ""

-- workaround to handle ghc's strange inability to deduce Show-instance

pc2 :: Show b => [b] -> IO ()
pc2 (b:bs) = putStrLn (pad 35 "" ++ take 35 (i $ show b)) >> pc2 bs
pc2 [] = putStrLn ""

pc1 :: Show a => [a] -> IO ()
pc1 (a:as) = putStrLn (pad 35 (i $ show a)) >> pc1 as
pc1 [] = putStrLn ""

pad :: Int -> String -> String
pad x str = take x (str ++ repeat ' ')

indent :: Int -> String -> String
indent x str = replicate x ' ' ++ str

{--
>>> report mws
M' "ALST" "":
     WN "Al Este" 0                     WN "Alice White" 11859           NUPE!

M' "APL" "":
     WN "Abel\233" 0                    WN "Obalo" 12548                 NUPE!
                                        WN "Opolo" 2556

M' "ARNNS" "":
     WN "Aaron Wines" 0                 WN "Orion Wines" 1756            NAH!
 
M' "FNK" "":
     WN "Venec" 0                       WN "Finca 8" 9022                NO!
                                        WN "Vi\\241a Egu\\237a" 177132   NIX!

M' "FRNR" "":
     WN "F\246hrner" 0                  WN "Varner" 6742                 WHA?

M' "HTKPRR" "":
     WN "Haute Cabriere" 0              WN "Haute Cabri\\232re" 178424   YES!

M' "KMPL" "":
     WN "Kompoloi" 0                    WN "Campelo" 4842                Ei!

M' "KNST" "":
     WN "Kynast" 0                      WN "Gancedo" 3156                Non!

M' "PLTRNR" "":
     WN "Pelter Winery" 0               WN "Paltrinieri" 2729            maybe?

M' "PNN" "":
     WN "Beaune wine" 0                 WN "Banyan" 594                  n
                                        WN "BonAnno" 6462                n
                                        WN "Pinino" 5550                 n

M' "RFNS" "":
     WN "Reeve Wines" 0                 WN "Ravines" 3026                n

M' "RKRT" "":
     WN "Recaredo" 0                    WN "Recuerdo" 2805               YES
                                        WN "Ricordi" 4843

M' "SKNR" "":
     WN "Sakai Winery" 0                WN "Skinner" 3421                HA!

M' "SKSTN" "":
     WN "Zechstein" 0                   WN "Sexton" 17061                nw!

M' "SLRS" "":
     WN "21 Cellars" 0                  WN "18401 Cellars" 4415          yes?
                                        WN "4 Cellars" 8100              nah

M' "XTFJK" "XTFKK":
     WN "Ch\226teau-Figeac" 0           WN "Ch\\226teau Figeac" 188517   YES

M' "XTKLN" "":
     WN "Ch\226teau Golan" 0            WN "Ch\\226teau Calon" 188786    UH HUH!
                                        WN "Ch\\226teau Kalian" 188787

M' "XTKRNTTPTKS" "":
     WN "Ch\226teau Grand-Puy-Ducas     WN "Ch\\226teau Grand-Puy Duca   YES

M' "XTLFLLSKSS" "":
     WN "Ch\226teau L\233oville-Las     WN "Ch\\226teau L\\233oville L   YES

M' "XTLKFLR" "":
     WN "Ch\226teau La Gaffeli\232r     WN "Ch\\226teau la Gaffeli\\23   YES

M' "XTLKST" "":
     WN "Ch\226teau La Coste" 0         WN "Ch\\226teau La Caussade" 1   no cuz
                                        WN "Ch\\226teau la Coste" 1893   YES

M' "XTLTRPLNX" "XTLTRPLNK":
     WN "Ch\226teau La Tour Blanche     WN "Ch\\226teau la Tour Blanch   YES

M' "XTPRT" "":
     WN "Ch\226teau Bayard" 0           WN "Ch\\226teau Baret" 190400
                                        WN "Ch\\226teau Bourdieu" 1904
                                        WN "Ch\\226teau Briot" 190402

M' "XTSNTRSLN" "":
     WN "Ch\226teau Sainte-Roseline     WN "Ch\\226teau Sainte Roselin   YES

M' "XTTKLN" "":
     WN "Ch\226teau de Goulaine" 0      WN "Ch\\226teau de Go\\235lane   YES
                                        WN "Ch\\226teau du Glana" 1908

M' "XTTKRPKL" "":
     WN "Ch\226teau Ducru-Beaucaill     WN "Ch\\226teau Ducru Beaucail   YES

M' "XTTLLSLNR" "":
     WN "Ch\226teau de l'Oiselini\2     WN "Chateau de l'Oiselini\\232   YES

M' "XTTMRSN" "":
     WN "Ch\226teau de Marsannay" 0     WN "Ch\\226teau de Marsan" 191   YES

M' "XTTRNFN" "XTTRNFKN":
     WN "Ch\226teau de Rayne-Vignea     WN "Ch\\226teau de Rayne Vigne   YES

M' "XTTSTN" "":
     WN "Ch\226teau Doisy Da\235ne"     WN "Ch\\226teau Doisy-Da\\235n   YES

Huh, so: not bad! :D

We had 16 good matches out of 30 candidates.
--}
