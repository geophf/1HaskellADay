{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D01.Solution where

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

import Control.Monad.State   -- whoa: seriously?

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Logic.Frege ((-|))
import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows

-- first: find the aliases.

orphanedCountriesQuery :: Cypher
orphanedCountriesQuery =
   "match (c:Country) where not (:Continent)<--(c) return c"

-- how many orphaned countries are there?

data OrphanedCountry = 
   OrphanedCountry { name :: String , qid  :: Maybe String }
      deriving (Eq, Ord, Show)

instance FromJSON OrphanedCountry where
   parseJSON = withObject "Orphaned country" $ \v ->
      OrphanedCountry <$> v .: "name" <*> v .:? "qid"

orphanedCountries :: Endpoint -> IO (Set OrphanedCountry)
orphanedCountries = getGraphData orphanedCountriesQuery

-- and, more generally:

getGraphData :: Ord a => FromJSON a => Cypher -> Endpoint -> IO (Set a)
getGraphData cyph url =
   Set.fromList . map (head . row) . justRows <$> getGraphResponse url [cyph]

{--
>>> :set -XOverloadedStrings 
>>> graphEndpoint 
...
>>> let url = it

>>> orphanedCountries url orphanedCountriesQuery 
fromList []

hm. ... not good. ... Ah, because the row-type is [OrphanedCountry] (an Array)
not OrphanedCountry (a Value). ... added `head .` to the mapper.

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
   parseJSON = withObject "Continent" $ \v -> Continent <$> v .: "name"

storedContinents :: Endpoint -> IO (Set Continent)
storedContinents =  getGraphData "match (c:Continent) return c"

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
   parseJSON = withObject "Country" $ \v -> Country <$> v .: "name"

storedCountriesQuery :: Cypher
storedCountriesQuery = "match (c:Country) where (:Continent)<--(c) return c"

storedCountries :: Endpoint -> IO (Set Country)
storedCountries = getGraphData storedCountriesQuery

{--
>>> storedCountries url
fromList [Country {country = "Afghanistan"},Country {country = "Albania"},...]
>>> let countries = it
>>> Set.size countries
231

That's sallata tea ... I mean: countries!
--}

type AliasMap = Map Country (Set OrphanedCountry)

data CountryTriage = Triage {
      linked :: AliasMap,
      news   :: Map OrphanedCountry Continent,
      others :: Set OrphanedCountry
   }
   deriving (Eq, Ord, Show)

linkOrphanedCountries :: Set Continent -> Set Country -> Set OrphanedCountry 
                      -> CountryTriage
linkOrphanedCountries cs1 cs2 ocs =
   execState (loc' cs1 cs2 (Set.toList ocs))
             (Triage Map.empty Map.empty Set.empty)

loc' :: Set Continent -> Set Country -> [OrphanedCountry]
     -> CountryTriageState ()
loc' cs1 cs2 [] = return ()
loc' cs1 cs2 (o:cs) = orphanMatcher cs1 cs2 o >> loc' cs1 cs2 cs

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
ocs' _ cs name@"Bahamas" = addAlias cs "The Bahamas" name
ocs' _ cs name@"Bangladesh1" = addAlias cs "Bangladesh" name
ocs' _ cs name@"Cabo Verde" = addAlias cs "Cape Verde" name
ocs' _ cs name@"Cambodia1" = addAlias cs "Cambodia" name
ocs' _ cs name@"Cameroun" = addAlias cs "Cameroon" name
ocs' _ cs name@"Tchad" = addAlias cs "Chad" name
ocs' _ cs name@"Zaire" = addAlias cs "Democratic Republic of the Congo" name
ocs' _ cs name@"Misr" = addAlias cs "Egypt" name
ocs' _ cs name@"Abyssinia" = addAlias cs "Ethiopia" name
ocs' cnts _ name@"Guinea-Bissau" = newCountry cnts "Africa" name
ocs' _ cs name@"Ísland" = addAlias cs "Iceland" name
ocs' _ cs name@"Iraqi" = addAlias cs "Iraq" name
ocs' _ cs name@"Éire" = addAlias cs "Republic of Ireland" name
ocs' _ cs name@"Ireland" = addAlias cs "Republic of Ireland" name
ocs' _ cs name@"Ivory Coast" = addAlias cs "Côte d'Ivoire" name
ocs' _ cs name@"Црна Гора" = addAlias cs "Montenegro" name
ocs' _ cs name@"Κύπρος" = addAlias cs "Cyprus" name
ocs' _ cs name@"Al Maghrib" = addAlias cs "Morocco" name
ocs' _ cs name@"Македонија" = addAlias cs "North Macedonia" name
ocs' _ cs name@"People's Republic of China" = addAlias cs "China" name
ocs' _ cs name@"Sao Tome and Principe" = addAlias cs "São Tomé and Príncipe" name
ocs' _ cs name@"Slovenija" = addAlias cs "Slovenia" name
ocs' _ cs name@"St. Kitts and Nevis" = addAlias cs "Saint Kitts and Nevis" name
ocs' _ cs name@"St. Lucia" = addAlias cs "Saint Lucia" name
ocs' _ cs name@"St. Vincent and the Grenadines" =
   addAlias cs "Saint Vincent and the Grenadines" name
ocs' _ cs name@"Timor Leste" = addAlias cs "East Timor" name
ocs' _ cs name@"UAE" = addAlias cs "United Arab Emirates" name
ocs' _ cs name@"UK" = addAlias cs "United Kingdom" name
ocs' _ cs name@"USA" = addAlias cs "United States of America" name
ocs' _ cs name@"United States" = addAlias cs "United States of America" name
ocs' cnts _ name@"Palestine" = newCountry cnts "Asia" name

ocs' _ cs name = add2others name  -- the default -- poor Yugoslavia!

add2others :: String -> CountryTriageState ()
add2others n = get >>= \ct ->
   let o = others ct in put (ct { others = Set.insert (mkOC n) o })

addAlias :: Set Country -> String -> String -> CountryTriageState ()
addAlias cs countryName alias = get >>= \ct ->
   let lnks = linked ct in
   put (ct { linked = aliasInserter cs lnks alias countryName })

aliasInserter :: Set Country -> AliasMap -> String -> String -> AliasMap
aliasInserter cs am alias = 
   let oc = mkOC alias in
   maybe am
         (\country -> Map.insert country
                                 (maybe (Set.singleton oc)
                                        (Set.insert oc)
                                        (Map.lookup country am)) 
                                 am)
      . fetchCountry cs 

newCountry :: Set Continent -> String -> String -> CountryTriageState ()
newCountry ctns continentName countryName =
   get >>= \ct ->
   let mews = news ct
       mbcontinent = fetchContinent ctns continentName
       country = mkOC countryName
       newMap = maybe mews (flip (Map.insert country) mews) mbcontinent
   in  put (ct { news = newMap })

mkOC :: String -> OrphanedCountry
mkOC = flip OrphanedCountry Nothing

mkCountry :: String -> Country
mkCountry = Country

instance Monoid Country where
   mappend = undefined
   mempty = undefined

-- I think that it's stupid (in this case) that Maybe a requires a to be a 
-- monoid for Maybe a to be a monoid, but, hey, dems da rulz. le sigh.
-- ... MAYbe (geddit?) it's not a monoid, then, but an ... unoid? idk.

fetchCountry :: Set Country -> String -> Maybe Country
fetchCountry = fetcher mkCountry

fetcher :: Monoid m => Ord m => (String -> m) -> Set m -> String -> Maybe m
fetcher f cs c =
   let ans = f c
   in  Set.member ans cs -| return ans

{--
>>> aliasInserter Set.empty Map.empty "United States" "United States of America"
fromList []

>>> let am = aliasInserter (Set.singleton $ Country "United States of America")
                           Map.empty "United States" "United States of America"
>>> aliasInserter (Set.singleton $ Country "United States of America") am "USA"
                  "United States of America"
fromList [(Country {country = "United States of America"},
           fromList [OrphanedCountry {name = "USA", qid = Nothing},
                     OrphanedCountry {name = "United States", qid = Nothing}])]
--}

fetchContinent :: Set Continent -> String -> Maybe Continent
fetchContinent =   -- same!
   fetcher Continent

-- which means we need to make Continent monoidal

instance Monoid Continent where
   mempty = undefined
   mappend = undefined

{--
This is as far as we get today! Tomorrow we'll look at updating the graph
from the results of this triage.

How many new countries did you find? How many aliases did you create?
How many `countries` (that aren't countries (any more (Yugoslavia))) did
you choose to sideline?

FROM THE TOP! :<

>>> graphEndpoint 
...
>>> let url = it
>>> orphanedCountries url
...
>>> let ocs = it
>>> storedContinents url
>>> let contis = it
>>> storedCountries url
>>> let countrs = it
>>> linkOrphanedCountries contis countrs ocs 
--}

triageCountries :: IO CountryTriage
triageCountries =
   graphEndpoint         >>= \url ->
   storedContinents url  >>= \contis ->
   storedCountries url   >>= \countrs ->
   orphanedCountries url >>=
   return . linkOrphanedCountries contis countrs

{--
>>> triageCountries 
Triage {linked = {(Country {country = "Bangladesh"},
                           {[OrphanedCountry {name = "Bangladesh1", qid = Nothing}]),...,
                  (Country {country = "United Arab Emirates"
                           {[OrphanedCountry {name = "UAE", qid = Nothing}]),
                  (Country {country = "United Kingdom"
                           {[OrphanedCountry {name = "UK", qid = Nothing}]),
                  (Country {country = "United States of America"
                           {[OrphanedCountry {name = "USA", qid = Nothing},
                             OrphanedCountry {name = "United States", qid = Nothing}])], 

        news = {[(OrphanedCountry {name = "Guinea-Bissau", qid = Nothing},
                  Continent {continent = "Africa"}),
                 (OrphanedCountry {name = "Palestine", qid = Nothing},
                  Continent {continent = "Asia"})], 

        others = {[OrphanedCountry {name = "Ansarullah", qid = Nothing},
                   OrphanedCountry {name = "Hezbollah", qid = Nothing},
                   OrphanedCountry {name = "Saint Helena, Ascension and Tristan da Cunha", qid = Nothing},
                   OrphanedCountry {name = "Yugoslavia", qid = Nothing},
                   OrphanedCountry {name = "\922\973\960\961\959\962", qid = Nothing}]}

so, to answer the questions:

>>> let triage = it
>>> Map.size (linked triage)
24
>>> Map.size (news triage)
2
>>> Set.size (others triage)
5

--}
