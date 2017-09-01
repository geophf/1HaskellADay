{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D01.Exercise where

import Data.Aeson
import Data.Map (Map)
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git repository

import Data.Percentage
import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint
import Y2017.M08.D31.Exercise (url)

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

type Country = String
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
qidFromURL url = undefined

countryMap :: SPARQL -> IO (Map Country QID)
countryMap = undefined

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

popByCountry :: SPARQL -> IO (Map Country Population)
popByCountry query = undefined

-- Part II (or: Part, the Second)

-- Now, compute the percentage of FB users by country per its population

-- Recall that FB users by country was yesterday's exercise

percFBuserPerCountry :: SPARQL -> URL -> IO (Map Country Percentage)
percFBuserPerCountry query fbUsersURL = undefined

-- Question: which country has the highest percentage of FB users by population?
-- Which country has the lowest percentage of FB users by population?
