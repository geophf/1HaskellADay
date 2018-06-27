{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Y2018.M06.D27.Solution where

{--
Here's a 10-megabyte, one-line, JSON-parsing problem, that also requires
parsing the parsed JSON. I could be cruel and say "You'll see!" because you
will, but let's examine one of the key-value pairs of this megalithic line of
JSON and tease the problem apart.

Our first key-value pair is as follows:

"0": "[(0, (0, 0.9999999403953552)), (9, (355, 0.6180121898651123)), 
       (3, (622, 0.6893148422241211)), (1, (12501, 0.7031754851341248)), 
       (8, (12809, 0.6247819662094116)), (6, (17479, 0.6647325754165649)), 
       (11, (30414, 0.6125686168670654)), (2, (32139, 0.6946772933006287)), 
       (12, (32591, 0.6095214486122131)), (4, (33350, 0.6777978539466858)), 
       (5, (33737, 0.6767839193344116)), (7, (34455, 0.6547491550445557)), 
       (13, (34895, 0.6010595560073853)), (10, (35268, 0.6166083812713623)), 
       (14, (35651, 0.5981945991516113))]"

And I'm like: 'srsly?' And the JSON is like: 'yup, srsly. deal.'

Salty JSON, I declare!

So, there are actually multiple problems here. One problem is to parse the
JSON. That should be simple enough, right? Because it resolves, simply, to
Map String String.
--}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

readMap :: FilePath -> IO (Map String String)
readMap = fmap (fromJust . decode) . BL.readFile

-- this big honkin' map is located at:

exDir, honkin :: FilePath
exDir = "Y2018/M06/D27/"
honkin = "corp_index_lsi0.json"

{-- 
How many elements does this map have?

>>> honk <- readMap (exDir ++ honkin)
>>> length honk
19999

Not bad, not bad!

Okay, examining the structure of our Map String String, we see it's really
a graph of graphs which we can represent as a data structure. Let's do that:
--}

type Vertex = Integer
type Strength = Double
type Relations = Map Vertex (Vertex, Strength)

-- so we see our Codex is a Vertex -> Relations mapping

type Codex = Map Vertex Relations

-- So, transform the Map String String to a Codex

mapping2Codex :: Map String String -> Codex
mapping2Codex = Map.map mapify . Map.mapKeys read

mapify :: String -> Relations
mapify = Map.fromList . read

{--
>>> codex = mapping2Codex honk
>>> head (Map.toList codex)
(0,fromList [(0,(0,0.9999999403953552)),(1,(12501,0.7031754851341248)),
             (2,(32139,0.6946772933006287)),(3,(622,0.6893148422241211)),
             (4,(33350,0.6777978539466858)),(5,(33737,0.6767839193344116)),
             (6,(17479,0.6647325754165649)),(7,(34455,0.6547491550445557)),
             (8,(12809,0.6247819662094116)),(9,(355,0.6180121898651123)),
             (10,(35268,0.6166083812713623)),(11,(30414,0.6125686168670654)),
             (12,(32591,0.6095214486122131)),(13,(34895,0.6010595560073853)),
             (14,(35651,0.5981945991516113))])

Waitaminit! Waitaminit! Waitaminit!

You're telling me that the relationships type is not a mapping, but is more
correctly an array-type? That is to say: are there any elements that

1) don't have 15 elements
2) the indices for all these elements fall in the range 0-14?

Write a function isArrayed that proves or disproves the above two statements
--}

isArrayed :: Codex -> Bool
isArrayed = all arrayedRow . Map.elems

arrayedRow :: Relations -> Bool
arrayedRow row = length row == 15 
              && all (\idx -> idx < 15 && idx >=0) (Map.keys row)

{--
>>> isArrayed codex
False

Nope. That is not the case that all the elements of the Codex fall perfectly
into place. Find all the rows that do not match this pattern. How many not-
perfectly-arrayed rows are there?
--}

notPerfectlyArrayedRows :: Codex -> Codex
notPerfectlyArrayedRows = Map.filter (not . arrayedRow)

{--
>>> exCodex = notPerfectlyArrayedRows codex 
>>> length exCodex 
24

24 rows don't fit the arrayed-pattern. Is there a theme for these exceptional
rows? What could the theme be? Are there any of these exceptional rows that
don't have indices that aren't immediate successors? Are there any that are
longer than 15 elements? Let's find out.

Convert this sub-codex to a mapping of vertices -> lengths
--}

type Length = Int

eltLengths :: Codex -> Map Vertex Length
eltLengths = Map.map length

{--
>>> eltLengths exCodex 
fromList [(1139,0),(2872,0),(3331,0),(5571,0),(7548,0),(7664,0),(7781,0),
          (8036,0),(8084,0),(8160,0),(8161,0),(8289,0),(8384,0),(8413,0),
          (8467,0),(8469,0),(8507,0),(9841,0),(15406,0),(15807,0),(15998,0),
          (16974,0),(17775,0),(19514,0)]

Well! I'll be! These vertices, without exception, are empty leaf nodes. Proof:

>>> all (== 0) (Map.elems (eltLengths exCodex))
True

A few things to note about this JSON file. Take, for example:

>>> codex Map.! 5234
fromList [(0,(5234,1.0)),(1,(52422,0.8558297157287598)),
          (2,(272989,0.8420774936676025)),(3,(169420,0.8344746232032776)),
          (4,(5332,0.8296037912368774)),(5,(34932,0.817562460899353)),
          (6,(101789,0.8140842914581299)),(7,(72925,0.8113268613815308)),
          (8,(124921,0.7994502782821655)),(9,(101781,0.7962022423744202)),
          (10,(59450,0.7953096628189087)),(11,(72928,0.7881829738616943)),
          (12,(146936,0.7857747673988342)),(13,(7030,0.7848776578903198)),
          (14,(72918,0.7823269963264465))]

1. it's a subgraph: there are vertices mentioned that are greater than the
   indexed vertices.
2. It's not a DAG (Directed Acyclic Graph). In fact, every vertex's first
   relation is to itself.
3. All relations are indexed from strongest relation (itself, which is most
   often a 1.0 relation strength) to weakest, so indexing the relationshs
   shows strength.

A thought. If we remove the first element, does this graph become a DAG? How
would we find this out?

Question: how many unique vertices are there? (Note, given 2. above, you can
compute this from Map.elems)
--}

uniqueVertices :: Codex -> Set Vertex
uniqueVertices = foldr (Set.union . Set.fromList . map fst . Map.elems) Set.empty . Map.elems

{--
>>> length (uniqueVertices codex)
106709

So, in 20,000 rows we have 106,709 vertices. Sweet!

Okay, back to the DAG-question. If every element is greater than the first
for every Relations row, that may be indicative of a DAG. Is this the case?
--}

monotonicallyIncreasing :: Relations -> Bool
monotonicallyIncreasing  = mti . map fst . Map.elems

mti :: [Vertex] -> Bool
mti [] = True
mti (h:t) = all (> h) t

{--
>>> all monotonicallyIncreasing (Map.elems codex)
False

Nope. Haskell says no. How many are not monotonically increasing rows?

>>> length (Map.filter (not . monotonicallyIncreasing) codex)
12295

Ooh! A lot! A sample looks like ...

>>> head . Map.toList $ Map.filter (not . monotonicallyIncreasing) codex
(37,fromList [(0,(11665,1.0)),(1,(11969,1.0)),(2,(3682,1.0)),(3,(1747,1.0)),
              (4,(10900,1.0)),(5,(11146,1.0)),(6,(11368,1.0)),(7,(18637,1.0)),
              (8,(37,1.0)),(9,(2788,1.0)),(10,(12536,1.0)),(11,(12272,1.0)),
              (12,(3967,1.0)),(13,(3060,1.0)),(14,(2518,1.0))])

Okay, then. It's possible that the graph is not a DAG.

Tomorrow we'll translate this graph into a graph database. Ooh! Pretteh graphs!
--}
