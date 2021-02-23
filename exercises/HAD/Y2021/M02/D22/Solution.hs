{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Y2021.M02.D22.Solution where

{--
OKAY!

We now have a graph-store of wines, wineries, reviews, prices, scores, ...

We good?

We GOOD!

So, there are several directions we can go from here.

One Direction, ...

... eheh, ...

One Direction is to track which Boy Bands buy what kinds of wines and start
to analyze why teen girls went crazy over that band for, oh, about a year
before they fell into utter obscurity.

But that's not the direction we're going today.

Another direction is to do some natural language processing on the reviews
and start to build models of wines, preferences, what-have-you, to build a
recommendation system, pairing wines with people and foods.

Not a direction we're taking today, either.

Another direction is to take that wikidata and see how we can find 'soft
aliases' to the wineries in our graph store to match wikidata wineries
and, consequently, marry their locations to the wineries in our graph-store.

Let's do that, with an eye toward building a geodesic model of wines and
wineries, ... for funsies.

We did, before collect wineries by country. Perhaps that can help to narrow
down our search of the wikidata wineries-by-country set.

So, the approach here is we're going to use various matching techniques,
including artificial artificial intelligence (a term made popular by google),
to winnow our wikidata list of wineries into our wineries on the graph-store.

What is the distinguishing characteristic of the matched wineries (other than
that they have matched)? The matched wineries have lat/longs in the graph-
store.

So! Finding what matched on the graph-side is simple. How do we know that a
wikidata winery has been matched?

Hm.

Back to aliasing.

We've aliased matched countries before, using the relation ALIAS_OF. Let's
continue in that vein for wineries that don't have an exact match, therefore
masking out already-matched wineries on the wikidata-side.

Using this approach, we now can apply different kinds of matchers, piecemeal,
and recover the most-recent state of matching the next day after leaving off
the previous approach.

Cool beans!

Okay, step zero. We need to collect all wineries from both sources and
eliminate already matched ones. Since the previous match was exact, the
elimination is reductive. But, also, we may as well also eliminate any aliased
wineries, even though, on this first day, there are none, just so that this
exercise works every time we take on a new matching technique.

So, today's Haskell exercise will be marshalling our data-sets. We'll look at
various matchers going forward.
--}

import Control.Arrow (second)

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Data.Aeson.WikiDatum

import Graph.Query
import Graph.JSON.Cypher
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2021.M01.D21.Solution (Idx)
import Y2021.M01.D22.Solution    -- for wikidata wineries
import Y2021.M01.D26.Solution (ByCountry, WineriesByCountry, Country)

{--
import Y2021.M01.D21.Solution    -- remember 'Sweet Cheeks' winery? lol ;)

but it's fun looking over that 'old' code; seeing how much I have rolled 
into the 1HaskellADay standard library set.

... also, Wineries in the graph-store are now more complex.
--}

import Y2021.M01.D25.Solution hiding (toPair)   -- country-alias resolver
import Y2021.M01.D26.Solution (wikiWineriesByCountry)
import Y2021.M01.D29.Solution hiding (toPair)   -- Namei

{--
First up: let's grab our country aliases

>>> graphEndpoint >>= countryAliases
fromList [(WC "German Democratic Republic",Neo "Germany"),
          (WC "United Kingdom",Neo "England"),
          (WC "United States of America",Neo "US")]

>>> let ca = it

Next, let's grab out wikidata:

>>> readWineries (wineriesDir ++ wineriesJSON)
...
>>> let wikiw = it
>>> head (Map.toList wikiw)
("21 Cellars",Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q4630984", 
                                   name = "21 Cellars"},
                      country = WD {qid = "http://www.wikidata.org/entity/Q30", 
                                    name = "United States of America"},
                      location = point({ latitude: 47.2675, longitude: -122.471 })})

Now, let's normalize the wiki-countries to graph-countries
--}

normalizeWikiCountry :: Map WikiCountry Neo4jCountry -> Winery -> Winery
normalizeWikiCountry countryMap w = 
   let (WD qid c) = country w
       (NC (Neo c'))   = countryAliasResolver countryMap (WC (Wiki c))
       wdc        = WD qid c'
   in  w { country = wdc }

-- Wait.

-- We have to redo our contryAliases to return the WikiDatum for countries
-- and their aliases so we can do a proper substitution here.

-- No, because aliased countries' QNames aren't in the graph-store. So we'll
-- use the QNames in wiki, because they do match. ... nvrmnd.

{--
>>> let normWikiw = Map.map (normalizeWikiCountry ca) wikiw
>>> head (Map.toList normWikiw)
("21 Cellars",Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q4630984", 
                                   name = "21 Cellars"}, 
                      country = WD {qid = "http://www.wikidata.org/entity/Q30", 
                                    name = "US"}, 
                      location = point({ latitude: 47.2675, longitude: -122.471 })})

BOOM!

Let's group these by country ... or ... ByCountry. Eheh ;)

>>> let wwbc = wikiWineriesByCountry normWikiw
>>> second (take 3 . Set.toList) (head (Map.toList wwbc))
("Argentina",[Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q2829326", 
                                   name = "Al Este"},
                      country = WD {qid = "http://www.wikidata.org/entity/Q414", 
                                    name = "Argentina"},
                      location = point({ latitude: -38.8, longitude: -62.68333333 })},
              Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q5731065", 
                                   name = "Bodega B\243rbore"}, 
                      country = WD {qid = "http://www.wikidata.org/entity/Q414", 
                                    name = "Argentina"}, 
                      location = point({ latitude: -31.54805556, longitude: -68.32722222 })}])

Turns out there's only 2 wineries listed in Argentina with lats/longs in
wikidata.

Now let's grab the wineries from the graph database, which may optionally
have a QName and a lat/long, which we'll have to parse (kinda) (not if I can
help it), also, we wish to associate the id in the graph database with this
object, as well as its country.
--}

wineriesQuery :: Cypher
wineriesQuery =
   T.pack (unwords ["MATCH (c:Country)<--()<--(w:Winery)",
                    "RETURN id(w) as id, w.name as winery_name,",
                    "w.qid as winery_qid, c.name as country_name,",
                    "c.qid as country_qid, w.location.longitude as lon,",
                    "w.location.latitude as lat"])

-- what we get back from that is:

data MbWikiDatum = MWD (Maybe Qname) Name   -- not sure if we have a QName
   deriving (Eq, Ord, Show)

data NeoWinery = NeoWinery Idx MbWikiDatum MbWikiDatum (Maybe LongLat)
   deriving (Eq, Ord, Show)

instance Namei NeoWinery where
   namei (NeoWinery _ (MWD _ n) _ _) = n

row2NeoWinery :: [Value] -> Maybe (Name, NeoWinery)
row2NeoWinery [idx, wn, wqid, cn, cqid, lon, lat] =
   RR.fromJSON1 idx  >>= \i ->
   RR.fromJSON1 wn   >>= \na ->
   RR.fromJSON1 wqid >>= \wid ->
   RR.fromJSON1 cn   >>= \cna ->
   RR.fromJSON1 cqid >>= \cid ->
   RR.fromJSON1 lon  >>= \llon ->
   RR.fromJSON1 lat  >>= \llat ->
   return (cna, NeoWinery i (MWD wid na) (MWD cid cna) (pointFrom llon llat))

pointFrom :: Maybe Double -> Maybe Double -> Maybe LongLat
pointFrom mbllon mbllat = Point <$> mbllon <*> mbllat

-- The reason for row2NeoWinery is because we wish to build a 
-- ByCountry NeoWinery map so it returns Maybe (country-name, winery))

-- and with that, we can do this:

graphWineries :: Endpoint -> IO (ByCountry NeoWinery)
graphWineries url = RR.multimapBy row2NeoWinery
   <$> getGraphResponse url [wineriesQuery]

{--
>>> graphEndpoint >>= graphWineries
...
>>> let gws = it
>>> second (take 2 . Set.toList) (head $ Map.toList gws)
("Argentina",[NeoWinery 480 (MWD Nothing "Kirkland Signature")
                            (MWD (Just "http://www.wikidata.org/entity/Q414") "Argentina") 
                            Nothing,
              NeoWinery 486 (MWD Nothing "Felix Lavaque") 
                            (MWD (Just "http://www.wikidata.org/entity/Q414") "Argentina") 
                            Nothing])

The thing is, we don't actually need maps, ... yet. We need maps once we've 
eliminated already-matched wineries. So, let's convert our maps to sets.
--}

map2set :: Ord a => Map k (Set a) -> Set a
map2set = Set.unions . Map.elems

{--
>>> let gwss = map2set gws
>>> let wwbcs = map2set wwbc
>>> (Set.size gwss, Set.size wwbcs)
(16961,605)

That about jibes with my recollection.

NOW, let's eliminate matches from both sets. Which means we have to find the
matches (by exact-match on their names).
--}

nameis :: Namei a => Set a -> Set Name
nameis = Set.map namei

exactMatches :: Set NeoWinery -> Set Winery -> Set Name
exactMatches (nameis -> neos) (nameis -> winos) = Set.intersection neos winos

{--
>>> let em = exactMatches gwss wwbcs
>>> Set.size em
124
--}

-- Remove those exact matches from both the neo-wineries and wiki-wineries

removeMatches :: Namei a => Set Name -> Set a -> Set a
removeMatches duplicates = Set.filter notNamedIn
   where notNamedIn = flip Set.notMember duplicates . namei

-- removeMatches is a Set.removeBy-function

{--
>>> let smgwss = removeMatches em gwss
>>> Set.size smgwss 
16836
>>> let smwwbcs = removeMatches em wwbcs 
>>> Set.size smwwbcs 
481

Okay. Now let's remove any aliased wineries, ... that we will eventually be
having.
--}

aliasedWineriesQuery :: Cypher
aliasedWineriesQuery = T.concat ["MATCH (aw:AliasedWinery)--(w:Winery) ",
                                 "RETURN aw.name, w.name"]

-- Grab these aliases:

data WikiWinery = WW (Wiki Name)
   deriving (Eq, Ord, Show)

instance Namei WikiWinery where
   namei (WW (Wiki n)) = n

data Neo4jWinery = NW (Neo4j Name)
   deriving (Eq, Ord, Show)

instance Namei Neo4jWinery where
   namei (NW (Neo n)) = n

type AliasedWineries = Map WikiWinery Neo4jWinery

toPair :: [Value] -> Maybe (WikiWinery, Neo4jWinery)
toPair [a,b] = RR.fromJSON1 a >>= \w ->
               RR.fromJSON1 b >>= \n ->
               return (WW (Wiki w), NW (Neo n))

aliasedWineries :: Endpoint -> IO AliasedWineries
aliasedWineries url = RR.mapBy toPair
                  <$> getGraphResponse url [aliasedWineriesQuery]

{--
>>> graphEndpoint >>= aliasedWineries
fromList []
>>> let awm = it

... as we currently have no aliased wineries, this result makes sense.

Now we delete the aliases from the wiki winery set and the wineries aliased
from the neo4j graph-store set as before.

And let's bring it all together.
--}

wineriesWIP :: FilePath -> Endpoint -> IO (Set Winery, Set NeoWinery)
wineriesWIP wikiFile url =
   countryAliases url    >>= \ca ->
   readWineries wikiFile >>= \wikiw ->
   graphWineries url     >>= \gws ->
   aliasedWineries url   >>= \awm ->
   let normWikiw = Map.map (normalizeWikiCountry ca) wikiw
       wwbc = wikiWineriesByCountry normWikiw 
       gwss = map2set gws
       wwbcs = map2set wwbc
       em = exactMatches gwss wwbcs
       smgwss = removeMatches em gwss
       smwwbcs = removeMatches em wwbcs 
       wikis = removeMatches (nameis $ Map.keysSet awm) smwwbcs
       neos = removeMatches (nameis . Set.fromList $ Map.elems awm) smgwss
   in  return (wikis, neos)

{--
>>> graphEndpoint >>= wineriesWIP (wineriesDir ++ wineriesJSON)
fromList [...]
>>> let (wikis, neos) = it
>>> (Set.size wikis, Set.size neos)
(481,16836)

There you go! A set-up of wineries to winnow down using name-matching.
--}
