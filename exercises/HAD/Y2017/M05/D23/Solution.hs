module Y2017.M05.D23.Solution where

import Control.Arrow ((&&&), second)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import qualified Data.MultiMap as MM
import Y2017.M05.D22.Solution

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

-- better to define languageTree first, then answer the following

languageRoots :: [Language] -> Set Root
languageRoots = Map.keysSet . languageTree

{--
>>> length . languageRoots <$> queryLangs 
320

e.g.: {"!Kung","Abazgi languages","Afro-Asiatic languages","Aiki language",...}
--}

languageRootsOf :: [Language] -> String -> [Root]
languageRootsOf langs myLang = fromMaybe [] (Map.lookup myLang (invmap langs))

{--
>>> (`languageRootsOf` "English") <$> queryLangs 
[]

>>> (`languageRootsOf` "Welsh") <$> queryLangs 
["Brythonic languages","Western Brittonic languages"]

>>> (`languageRootsOf` "Japanese") <$> queryLangs 
["Japonic languages","Sino-Xenic"]

>>> (`languageRootsOf` "Urdu") <$> queryLangs 
["Hindi-Urdu","Hindi-Urdu"]

>>> (`languageRootsOf` "Hawaiian") <$> queryLangs 
["Marquesic languages"]

... and then, the other way:

>>> maybe [] (map name) . Map.lookup "Marquesic languages" . languageTree <$> queryLangs 
["Marquesan language","Hawaiian"]

>>> maybe [] (map name) . Map.lookup "Brythonic languages" . languageTree <$> queryLangs 
["Cornish","Breton","Welsh"]
--}

invmap :: [Language] -> Map String [Root]
invmap = mapFromMap (name &&& root)

languageTree :: [Language] -> Map Root [Language]
languageTree = mapFromMap (root &&& id)

mapFromMap :: Ord a => (Language -> (a, b)) -> [Language] -> Map a [b]
mapFromMap = MM.store . foldr (uncurry MM.insert) MM.empty <<- map

languageTrunk :: Map Root [Language] -> (Root, [Language])
languageTrunk = head . sortOn (Down . length . snd) . Map.toList

{--
>>> second length . languageTrunk . languageTree <$> queryLangs 
("Bantu languages",37)

e.g.:

>>> second (take 5 . map (head . words . name)) . languageTrunk . languageTree <$> queryLangs 
("Bantu languages",["Vidunda","Kami","Bube","Doe","Yeyi"])
--}
