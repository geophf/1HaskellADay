{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D01.Solution where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Control.Scan.CSV (rend)
import Data.Percentage
import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

import Y2017.M08.D31.Solution

{--
Today's Haskell problem is a two-parter ... with seventy-three subsections.

(I love Rodney Dangerfield in "Back to School")

Yesterday, we saw Facebook users by country. Great. India wins! Go India!

But what are the PERCENTAGES of use per country by that country's population?

THAT is the question. Not the 'to be or not to be'-question, because that's
a rhetorical question by a wussy king-to-be who did nothing and let Ophelia die.

I'm just saying what you're all thinking, suffering through watching that play.

So, the countries stored in wikidata are:
--}

type SPARQL = String

countryQ :: SPARQL
countryQ = unlines ["SELECT ?country ?countryLabel WHERE {",
   "?country wdt:P31 wd:Q6256 .",
   "SERVICE wikibase:label {",
     "bd:serviceParam wikibase:language \"en\" .",
   "}",
   "}"]

-- First part (or: "Part the First") reify countries -> Qids to a map given

type URL = String

data CountryID = CountryID { ident :: URL, country :: Country }
   deriving (Eq, Ord, Show)

instance FromJSON CountryID where
   parseJSON o = CountryID <$> parseVal o "country" <*> parseVal o "countryLabel"

{--
>>> (fmap (take 5 . reifyWikiResults) $ sparql countryQ) :: IO [CountryID]
[CountryID {ident = "http://www.wikidata.org/entity/Q16", country = "Canada"},
 CountryID {ident = "http://www.wikidata.org/entity/Q17", country = "Japan"},
 CountryID {ident = "http://www.wikidata.org/entity/Q20", country = "Norway"},
 CountryID {ident = "http://www.wikidata.org/entity/Q27", country = "Ireland"},
 CountryID {ident = "http://www.wikidata.org/entity/Q28", country = "Hungary"}]

but you need just the Qid, not the URL of the Qid. Fix that.
--}

type QID = String

qidFromURL :: URL -> QID
qidFromURL = last . rend '/'

countryMap :: SPARQL -> IO (Map Country QID)
countryMap =
   fmap (Map.fromList . map (country &&& qidFromURL . ident) . reifyWikiResults)
      . sparql

{--
>>> countryMap countryQ
fromList [("Afghanistan","Q889"),("Albania","Q222"),("Algeria","Q262"),
          ("Andorra","Q228"),("Angola","Q916"),("Antigua and Barbuda","Q781"),
          ("Argentina","Q414"),("Armenia","Q399"),("Australia","Q408"),...]
--}

-- So, if the wikidata query to fetch a set of countries' populations is:

populationQ :: SPARQL
populationQ = unlines ["SELECT DISTINCT ?countryLabel ?population",
  "{",
  "?country wdt:P31 wd:Q6256 ;",
           "wdt:P1082 ?population .",
  "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" }",
  "}"]

-- Use the same approach as before to map countries to their populations,
-- constructing whatever intermediate forms necessary

type Population = Integer

data CountryPop = PopCountry { country' :: Country, pop :: String }
   deriving (Eq, Ord, Show)

instance FromJSON CountryPop where
   parseJSON o = PopCountry <$> parseVal o "countryLabel"
                            <*> parseVal o "population"

popByCountry :: SPARQL -> IO (Map Country Population)
popByCountry =
   fmap (Map.fromList . map (country' &&& read . pop) . toCountryPop)
      . sparql

toCountryPop :: ByteString -> [CountryPop]
toCountryPop = reifyWikiResults

{--
>>> popByCountry populationQ 
fromList [("Afghanistan",31822848),("Albania",3020209),..,("Zimbabwe",14149648)]
--}

-- Part II (or: Part, the Second)

-- Now, compute the percentage of FB users by country per its population

-- Recall that FB users by country was yesterday's exercise

percFBuserPerCountry :: SPARQL -> URL -> IO (FBusers Percentage)
percFBuserPerCountry query fbUsersURL =
   popByCountry query >>= \popMap  ->
   fmap (Map.fromList
       . map (fst &&& (P . ((%) . snd <*> mapper popMap . fst)))
       . Map.toList) $ readFBusers fbUsersURL

-- we need to finesse the country name from what Wikidata has to what we have.

mapper :: Map Country Population -> Country -> Population
mapper m "UK"  = mapper m "United Kingdom"
mapper m "USA" = mapper m "United States of America"
mapper m k     = fromMaybe (error ("No country " ++ k ++ " in wikidata set."))
                           (Map.lookup k m)

{--
First run:

*Y2017.M09.D01.Solution> percFBuserPerCountry populationQ url ~>
*** Exception: No country UK in wikidata set.

... adding in UK ...

Second run:

*Y2017.M09.D01.Solution> percFBuserPerCountry populationQ url ~>
*** Exception: No country USA in wikidata set.

... changing argument "US" to "USA" ...

Third run:

>>> percFBuserPerCountry populationQ url
fromList [("Brazil",69.37%),("India",19.07%),("Indonesia",50.42%),
          ("Mexico",65.12%),("Philippines",68.32%),("Thailand",86.45%),
          ("Turkey",73.46%),("UK",67.58%),("USA",74.08%),("Vietnam",69.79%)]

Sweet!
--}

-- Question: which country has the highest percentage of FB users by population?
-- Which country has the lowest percentage of FB users by population?

{--
>>> fmap ((head &&& last) . sortOn snd . Map.toList) $ percFBuserPerCountry populationQ url
(("India",19.07%),("Thailand",86.45%))

Yesterday, India was the winner, today India is the loser, and Thailand wins
the internets!

Question: What is a good way to represent these findings visually?

Perhaps this?

>>> percFBuserPerCountry populationQ url >>= chartFBusers "Y2017/M09/D01/fbusers.csv"
--}
