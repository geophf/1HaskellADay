module Control.Map where

-- some common map utilities

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

snarf :: (Ord c, Ord d) => (a -> Maybe (c,d)) -> [a] -> Map c (Set d)
snarf f = foldr inserter Map.empty . mapMaybe f

inserter :: (Ord a, Ord b) => (a, b) -> Map a (Set b) -> Map a (Set b)
inserter (a, b) m =
   Map.insert a (maybe (Set.singleton b) (Set.insert b) (Map.lookup a m)) m

{--
for example:

to make a simple multimap from a [(k,v)]

snarf return

If we have something like:

match :: Map Name Winery -> (Neo4jIxWinery, Country) -> Maybe (Country, IxWikiWinery)
match m (neo, c) = (c,) . flip IxWiki (ix neo) <$> Map.lookup (namei neo) m

see Y2021.M01.D29.Solution

then

snarf (match . byName) (translateByCountry)

gets you WineriesByCountry

(the ByCountry a map-type is used here)
--}

-- now when we which to preserve, say: two transactions that look the same, ...

snarfL :: Ord c => (a -> Maybe (c,d)) -> [a] -> Map c [d]
snarfL f = foldr inserterL Map.empty . mapMaybe f

inserterL  :: Ord a => (a, b) -> Map a [b] -> Map a [b]
inserterL (a, b) m = Map.insert a (maybe [b] (b:) (Map.lookup a m)) m
