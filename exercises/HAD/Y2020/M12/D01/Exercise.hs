{-# LANGUAGE OverloadedStrings #-}

module Y2020.M11.D30.Exercise where

{--
Why is the USA and the UK not part of NATO?

Well, actually, they are, but, our graph store from wikidata has some
problems of aliasing. In some cases it calls the country: "United States
of America" and in other cases, the country is called: "USA."

Same for the UK / United Kingdom ... (dare I say: England?)

Today's Haskell problem is that of this:

Okay, we now have data in our graph database: some data associated with the
country entity by name, and some associated with its alias. Find the problems
and correct the aliased data to point to the entity, then remove the (now-
orphaned) aliases.
--}

import Control.Monad.State

import Data.Aeson

import Data.Map (Map)
import Data.Set (Set)

import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows

-- first: find the aliases.

orphanedCountriesQuery :: Cypher
orphanedCountriesQuery =
   "match (c:Country) where not (:Continent)<--(c) return c"

-- how many orphaned countries are there?

data OrphanedCountry = 
   OrphanedCountry { name :: String, qid  :: Maybe String }
      deriving (Eq, Ord, Show)

instance FromJSON OrphanedCountry where
   parseJSON = undefined

orphanedCountries :: Endpoint -> IO (Set OrphanedCountry)
orphanedCountries = getGraphData orphanedCountriesQuery

-- and, more generally:

getGraphData :: FromJSON a => Cypher -> Endpoint -> IO (Set a)
getGraphData cyph url = undefined

{--
>>> :set -XOverloadedStrings
>>> graphEndpoint
...
>>> let url = it

>>> orphanedCountries url orphanedCountriesQuery
fromList []

hm. ... not good. ... Ah, because the row-type is [OrphanedCountry] (an Array)
not OrphanedCountry (a Value). Make sure to accommodate that in your solution.

>>> orphanedCountries url orphanedCountriesQuery
fromList [...,OrphanedCountry {name = "Yugoslavia", qid = Nothing}]

>>> let ocs = it
>>> Set.size ocs
34

... out of 265 countries.

Ugh. So we have our work cut out for us.

Also: look at this:

>>> Set.map qid ocs
fromList [Nothing]

Good to know this, post-query :/

Also-also: a country can have multiple aliases, e.g.: "United States" and "USA"
are aliases for "United States of America." Ugh.

Okay: enough "ugh"-ing. Let's get to work.

Now. How do we know of which country the orphaned country is an alias? Well,
there's a whole rabbit-hole of fun that wikidata provides: interlinks, but
we're not going down that rabbit hole.

No, not today.

Today, we'll manually create a partition: orphaned countries that are aliases
(and the country to which they alias), and orphaned countries that are not
aliases, because they are the only representing entity of that country, ...
[so we then need to de-enorphanitize that country and associate it to its
continent.

Okay, not a tall order. Not at all.

To do all that, we need links to already un-enorphanitzed countries and 
continents in the graph data-store.

Oh. Orphaned countries need to assign their alias to the proper country and
all the links to and from this (orphaned country) to that (proper country).

Get me?
--}

data Continent = Continent { continent :: String }
   deriving (Eq, Ord, Show)

instance FromJSON Continent where
   parseJSON = undefined

storedContinents :: Endpoint -> IO (Set Continent)
storedContinents = undefined

{--
>>> storedContinents url    
fromList [Continent {continent = "Africa"},
          Continent {continent = "Asia"},
          Continent {continent = "Europe"},
          Continent {continent = "North America"},
          Continent {continent = "Oceania"},
          Continent {continent = "South America"}]

Antarctica isn't there, because no airbases in Antarctica, because treaties.

Now do the same thing for countries that aren't orphaned.
--}

data Country = Country { country :: String }
   deriving (Eq, Ord, Show)

instance FromJSON Country where
   parseJSON = undefined

storedCountriesQuery :: Cypher
storedCountriesQuery = "match (c:Country) where (:Continent)<--(c) return c"

storedCountries :: Endpoint -> IO (Set Country)
storedCountries = undefined

{--
>>> storedCountries url
fromList [Country {country = "Afghanistan"},Country {country = "Albania"},...]
>>> let countries = it
>>> Set.size countries
231

That's sallata tea ... I mean: countries!

Okay!

Now let's link orphaned countries to proper countries
--}

type AliasMap = Map Country (Set OrphanedCountry)

data CountryTriage = Triage {
      linked :: AliasMap,
      news   :: Set OrphanedCountry,
      others :: Set OrphanedCountry
   }
   deriving (Eq, Ord, Show)

linkOrphanedCountries :: Set Continent -> Set Country -> Set OrphanedCountry 
                      -> CountryTriage
linkOrphanedCountries = undefined

{-- 
linkOrphanedCountries gives a map of links, the remaining (unlinked)
orphaned countries, and 'others' that aren't a country or aren't something
I want to think about for now.

BUT! To get to that point, we need to create a set of matchers that take an
OrphanedCountry and knows EXACTLY what to do with it and where to put it.
--}

type CountryTriageState a = State CountryTriage a

orphanMatcher :: Set Continent -> Set Country -> OrphanedCountry 
              -> CountryTriageState ()
orphanMatcher cnts cs = ocs' cnts cs . name

-- these helper methods may, well, ... help:

ocs' :: Set Continent -> Set Country -> String -> CountryTriageState ()
ocs' = undefined

add2others :: String -> CountryTriageState ()
add2others n = undefined

addAlias :: Set Country -> String -> String -> CountryTriageState ()
addAlias = undefined

newCountry :: Set Continent -> String -> String -> CountryTriageState ()
newCountry = undefined

mkOC :: String -> OrphanedCountry
mkOC = undefined

fetchCountry :: Set Country -> String -> Maybe Country
fetchCountry = undefined

fetchContinent :: Set Continent -> String -> Maybe Continent
fetchContinent = undefined

-- This is as far as we get today! Tomorrow we'll look at updating the graph
-- from the results of this triage.
