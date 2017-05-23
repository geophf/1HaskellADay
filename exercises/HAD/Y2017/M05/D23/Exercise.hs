module Y2017.M05.D23.Exercise where

import Data.Map (Map)
import Data.Set (Set)

-- below imports available via 1HaskellADay git repository

import Y2017.M05.D22.Exercise

{--
So I wished to analyze terror attack, in which country and for which intent.

Wikidata is all over the map in this, having categories that are not unified
by any class, and calling the same thing many different things: 'terror(ist)
attack' is not a category, but 'suicide bomber' is, which has nothing to do
with some of the recent modi of terrorists: using everyday tools or vehicles
to inflict death, dismemberment, and destruction.

Even 'death' is not a wikidata category, but 'homicide' is, but not all 
terrorist deaths are considered homicides, as they may be considered 'collateral
damage' to the intended attack.

And, as the media are quick to say 'do not label this as terrorism' giving 
reasons that 'the driver was intoxicated,' when the driver tested 0.0 on a 
Blood-Alcohol test (BAC/Blood-Alcohol Content), the data become muddled by
punditry.

All this is a suffix to the terrorist attack in Manchester, England, and, here,
a prefix to say that data science and data analytics is not straightforward
when the topics being analyzed are devisive, such as: poverty statistics, crime
by race, wages by gender, or terrorism.

So, wikidata: some topics are well-categorized and populated, some are scatter-
shot or woefully under- or mis-represented (example: do a SPARQL query to
get the populations of cities in the US State of Alaska: you get less than 10
results as of 2017-05-01, but there are over 144 cities on the wikipedia page
"List of Cities and their populations in Alaska").

What data sources do you use in your data analytics? There is the opendata.gov
project headed up by Princeton, I believe, but I am not familiar with that 
interface and every query I've submitted to that system has returned in a
'no result' or  'page not found'-error. That's discouraging. Do you have good
data sources I can poll?

Today's Haskell problem. Back to languages.

Given the list of languages (including their language-roots and number of
speakers) extracted from wikidata yesterday, compute:

1. the number of roots for the answer-set
2. The number of roots YOUR language has (list them)
3. The number of languages each root has
4. The root that has the most languages. What are they?
--}

type Root = String

languageRoots :: [Language] -> Set Root
languageRoots langs = undefined

languageRootsOf :: [Language] -> String -> [Root]
languageRootsOf langs myLang = undefined

languageTree :: [Language] -> Map Root [Language]
languageTree langs = undefined

languageTrunk :: Map Root [Language] -> (Root, [Language])
languageTrunk langTree = undefined
