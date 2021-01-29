{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Y2021.M01.D29.Exercise where

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
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D26.Solution (ByCountry, WineriesByCountry, Country)
import qualified Y2021.M01.D26.Solution as WbC
import Y2021.M01.D22.Solution (Wineries, Winery, winery, location)
import Y2021.M01.D21.Solution (Idx)

import Data.Aeson.WikiDatum

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Text as T

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
fetchIxWineries = undefined

-- Now, with these data, let's update our WineriesByCountry map

data IxWiki a = IxWiki a Idx
   deriving (Eq, Ord, Show)

type IxWikiWinery = IxWiki Winery

-- the country is the neo4j country.

instance Indexed IxWikiWinery where
   ix (IxWiki _ i) = i

type IxWikiWineries = ByCountry IxWikiWinery

indexWineriesMap :: MappedIxNeo4jWineries -> WineriesByCountry -> IxWikiWineries
indexWineriesMap = undefined

{--
The output of indexWineriesMap will be the neo4j country mapped to the wiki
winery that's been updated with its corrected country name and indexed with the
neo4j node id.

Also, if the Wiki winery has a corrected country in its key, correct that
in the wiki winery value, itself, as well.
--}

correctWikiWinery :: (Country, Winery) -> Winery
correctWikiWinery = undefined

-- Also, what may be helpful for the indexWineriesMap function, above,
-- is to translate the MappedIxNeo4jWineries into [(IxNeo4jWinery, Country)]

translateByCountry :: ByCountry a -> [(Set a, Country)]
translateByCountry = undefined

-- Now that you have corrected countries, let's reformulate the wiki wineries
-- mapped by name:

byName :: (Namei n, Foldable t) => t n -> Map Name n
byName = undefined

-- Why? so that we can find the wiki winery that matches the neo4j winery
-- then map the neo4j-country and -id to that matched wiki winery

match :: Map Name Winery -> (Neo4jIxWinery, Country) -> Maybe (Country, IxWikiWinery)
match = undefined

-- Are there any "No Country" wineries in the response? How many countries
-- are in the map? How many wineries?

-- Now that we have wikidata wineries with neo4j indices, let's update
-- the neo4j wineries with Qid's and geo-locations

updateWineryCypher :: IxWikiWinery -> Cypher
updateWineryCypher (IxWiki vin ix) =
   T.pack (concat ["MATCH (w:Winery) WHERE id(w) = ", show ix,
                   "SET w += { qid: '", T.unpack (qid $ winery  vin), 
                   "', location: ", show (location vin), " }"])
