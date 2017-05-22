module Y2017.M05.D22.Exercise where

import Data.Aeson   -- available via cabal
import Data.Set (Set)

-- the below imports are available via 1HaskellADay git repository:

import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

{--
We're going to examine language this week. Not the Haskell language, but
spoken languages (again, not Haskell).

So, looking at this on wikidata.org:

https://www.wikidata.org/wiki/Q13284

We see the Sora language (Q13284). That's not what interests me, however.

What interests me is the general structure of how languages are classified in
Wikidata. A language, such as Sora, is an instance of the class: Language, or:

https://www.wikidata.org/wiki/Q34770

(that's the 'q-name' for language)

And has a parent class of a language-root, in Sora's case is:

https://www.wikidata.org/wiki/Q33199 or Austro-Asiatic languages

But this language-family, itself, has a class:

https://www.wikidata.org/wiki/Q25295

(That is the 'q-name' of language family)

And has a 'number of speakers' property:

https://www.wikidata.org/wiki/Property:P1098

So, cool. Let's generalize from more than just the Sora language to all 
spoken languages

Let's query wikidata and see what spoken languages there are, what they are
subclasses of, and how many people speak those languages. So here is my SPARQL
query to ask this question:
--}

languagesSpeakersQuery :: String
languagesSpeakersQuery = unlines [
    "SELECT ?language ?languageLabel ?root ?rootLabel ?nspeakers WHERE {",
  "?language wdt:P31 wd:Q34770.",
  "?language wdt:P279 ?root.",
  "?language wdt:P1098 ?nspeakers.",
   "SERVICE wikibase:label {",
     "bd:serviceParam wikibase:language \"en\" .",
   "} }  ORDER BY ?nspeakers"]

{--
We'll look at languages, their roots, and their rarity later, but today, 
'simply' query the wikidata endpoint and answer the below questions:
--}

-- 1. how many unique langaguages are catalogued in wikidata?

data Language = Lang { name, root :: String, speakers :: Integer }
   deriving (Eq, Show)

instance FromJSON Language where
   parseJSON = undefined

uniqueLanguages :: [Language] -> Set String
uniqueLanguages langs = undefined

-- for your primary language, about how many speakers are there?

speakersOf :: [Language] -> String -> Integer
speakersOf langs yourlang = undefined

-- hint: Using the [Language]-structure may not be the best way to find this answer.
