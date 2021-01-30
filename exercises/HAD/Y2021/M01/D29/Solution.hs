{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}

module Y2021.M01.D29.Solution where

{--
So, yesterday, I was going to ask you to update wineries that we could match
exactly, that is to say: get the easy thing out of the way first, then, after
doing that, move onto the next easiest thing.

But I ran into an issue, trying that myself: the 'easy' thing wasn't as
straightforward as I thought it was.

Here's why: unicode.

They way Haskell translates a Text value to a unicoded string is, well,
unworkable for neo4j. And, let's face it, there are a lot of wineries that have
names that extend beyond straight-up ASCII.

So. A conundrum.

How do we solve it?

(I should write a book. No. Really)

Fortunately, we can 

1) download from the graph names-as-unicode that can match our wikidata
   names-as-unicode. So, exact unicode matches are possible.
2) neo4j labels every entity, nodes and relations, with identifiers (numbers),
   so, given that identifier that we can match to our local datum, we can
   update that graph-entity with our local data.

So. Let's do that with wineries.

But what's 'that'? And if 'that' is 'love,' then: 

"WHAT'S LOVE GOT TO DO WITH IT?"

Well, wine is love, so, there we go. Q.E.D.

Okay, specifically, 'that' is to match graph-identifiers to local data, then
to update the graph.

Let's do this.

First, we need to rework our neo4j extraction so that we now include identifiers.
--}

import Graph.Query
import Graph.JSON.Cypher (Cypher)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D27.Solution (correctedCountries)
import Y2021.M01.D26.Solution (ByCountry, WineriesByCountry, Country)
import qualified Y2021.M01.D26.Solution as WbC
import Y2021.M01.D22.Solution (Wineries, Winery(Winery), winery, location)
import Y2021.M01.D21.Solution (Idx)
import qualified Y2021.M01.D21.Solution as FJ

import Data.Aeson.WikiDatum

import Control.Arrow (second, (&&&))

import Data.Aeson (Value)

import Data.Foldable (toList)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Data.Tuple (swap)

-- so, we rewrite the Neo4jWinery

data Neo4jIx a = IxNeo a Idx
   deriving (Eq, Ord, Show)

type Neo4jIxWinery = Neo4jIx Name

class Indexed a where
   ix :: a -> Integer

class Namei a where
   namei :: a -> Name

instance Indexed Neo4jIxWinery where
   ix (IxNeo _ i) = i

instance Namei Neo4jIxWinery where
   namei (IxNeo n _) = n

fetchIxWineriesQuery :: Cypher
fetchIxWineriesQuery =
   "MATCH (c:Country)<--()<--(w:Winery) RETURN c.name, w.name, id(w)"

type MappedIxNeo4jWineries = ByCountry Neo4jIxWinery

fetchIxWineries :: Endpoint -> IO MappedIxNeo4jWineries
fetchIxWineries url =
   snarf (toPair . RR.row) . RR.justRows
         <$> getGraphResponse url [fetchIxWineriesQuery]

toPair :: [Value] -> Maybe (Name, Neo4jIxWinery)
toPair [country, winery, idx] =
   FJ.fromJSON1 country >>= \c ->
   FJ.fromJSON1 winery  >>= \w ->
   FJ.fromJSON1 idx     >>= \i ->
   return (c, IxNeo w i)

{--
>>> graphEndpoint
...
>>> let url = it
>>> fetchIxWineries url
fromList [("Argentina",fromList [IxNeo "2 Copas" 4305,IxNeo "25 Lagunas" 12348,...])]
>>> let ixneo = it
--}

-- Now, with these data, let's update our WineriesByCountry map

data IxWiki a = IxWiki a Idx
   deriving (Eq, Ord, Show)

type IxWikiWinery = IxWiki Winery

instance Indexed IxWikiWinery where
   ix (IxWiki _ i) = i

instance Namei Winery where
   namei = name . winery
instance Namei IxWikiWinery where
   namei (IxWiki w i) = namei w

type IxWikiWineries = ByCountry IxWikiWinery

indexWineriesMap :: MappedIxNeo4jWineries -> WineriesByCountry -> IxWikiWineries
indexWineriesMap mn = flip snarf (translateByCountry mn)
                    . match
                    . byName
                    . map (uncurry correctWikiWinery)
--                    . concatMap (sequence . second Set.toList)
                    . (>>= traverse Set.toList) -- via @oisdk
                    . Map.toList

snarf :: (Ord c, Ord d) => (a -> Maybe (c,d)) -> [a] -> Map c (Set d)
snarf f = foldr inserter Map.empty . mapMaybe f

inserter :: (Ord a, Ord b) => (a, b) -> Map a (Set b) -> Map a (Set b)
inserter (a, b) m =
   Map.insert a (maybe (Set.singleton b) (Set.insert b) (Map.lookup a m)) m

{--
The output of indexWineriesMap will be the neo4j country mapped to the wiki
winery that's been updated with its corrected country name and indexed with the
neo4j node id.

>>> correctedCountries url
...
>>> let wikiw = it
>>> let iwm = indexWineriesMap ixneo wikiw 
--}

-- Also, if the Wiki winery has a corrected country in its key, correct that
-- in the wiki winery value, itself, as well.

correctWikiWinery :: Country -> Winery -> Winery
correctWikiWinery c w@(Winery v c' ll) = Winery v (c' { name = c }) ll

-- Also, what may be helpful for the indexWineriesMap function, above,
-- is to translate the MappedIxNeo4jWineries into [(IxNeo4jWinery, Country)]

translateByCountry :: ByCountry a -> [(a, Country)]
translateByCountry m =
   [(a,b) | (s,b) <- map swap (Map.toList m), a <- Set.toList s]
   -- via @tim_1729 "insatiable mask wearer"

-- Now that you have corrected countries, let's reformulate the wiki wineries
-- mapped by name:

byName :: (Namei n, Foldable t) => t n -> Map Name n
byName = Map.fromList . map (namei &&& id) . toList

-- Why? so that we can find the wiki winery that matches the neo4j winery
-- then map the neo4j-country and -id to that matched wiki winery

match :: Map Name Winery -> (Neo4jIxWinery, Country) -> Maybe (Country, IxWikiWinery)
match m (neo, c) = (c,) . flip IxWiki (ix neo) <$> Map.lookup (namei neo) m

{-- 
Are there any "No Country" wineries in the response? 

>>> Map.keys iwm
["Argentina","Australia","Austria","Chile","France","Germany","Israel","Italy",
 "Moldova","New Zealand","Portugal","South Africa","Spain","US"]

So, ... NUPE!

How many countries are in the map? How many wineries?

Number of countries:

>>> Map.size iwm
14

>>> let wineries = Set.unions $ Map.elems iwm

>>> head $ Set.toList wineries 
IxWiki (Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q1061469", 
                             name = "Ch\226teau Mouton Rothschild"}, 
                country = WD {qid = "http://www.wikidata.org/entity/Q142", 
                              name = "France"}, 
                location = point({ latitude: 45.2153, longitude: -0.7686 })}) 
       1779

Note the unicode in the name. That's why we have the node index.

Number of wineries:

>>> Set.size wineries
124
--}
   
-- Now that we have wikidata wineries with neo4j indices, let's update
-- the neo4j wineries with Qid's and geo-locations
   
updateWineryCypher :: IxWikiWinery -> Cypher
updateWineryCypher (IxWiki vin ix) =
   T.pack (concat ["MATCH (w:Winery) WHERE id(w) = ", show ix,
                   "SET w += { qid: '", T.unpack (qid $ winery  vin),
                   "', location: ", show (location vin), " }"])

{--
>>> getGraphResponse url (Set.map updateWineryCypher wineries)
"{\"results\":[...],,\"errors\":[]}"

There we have it: a neo4j graph with wineries updated with geo-locations and
wikidata QNames! WOOT!
--}
