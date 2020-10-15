module Y2020.M10.D14.Solution where

{--
I really hate wikidata sometimes.

The query:

# Continents/Countries
SELECT ?continent ?continentLabel ?country ?countryLabel
     # ?region ?regionLabel # ?particularRegion ?particularRegionLabel 
WHERE 
{
  ?continent wdt:P31 wd:Q5107.
  ?country wdt:P31 wd:Q6256.
  # ?region wdt:P31 wd:Q82794.
  # ?region wdt:p642 ?continent.
  # ?particularRegion wdt:p361 ?region.
  ?country wdt:P361 ?continent.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}

says there are only two countries in Europe: Norway and Italy.

It also says there are only seventeen countries that are in continents in the
entire World. If you're not one of these seventeen countries, then, as Cee Lo
Green sings: "[Forget] you."

You see that some RDF triples are commented out. Removing the comment marker
reduces the result set to 0 countries and continents, even though manual
search shows otherwise.

[Forget] you, wikidata.

But, there is a wiki page that lists countries by continent. So I did a hand-
scrape of that page.

Today's Haskell problem, let's convert that scrape to Haskell data.
--}

import Data.List (isPrefixOf, stripPrefix)

import Data.Map (Map)
import qualified Data.Map as Map

workingDir :: FilePath
workingDir = "Y2020/M10/D14/"

cbc :: FilePath
cbc = "countries.txt"

type Continent = String
type Country = String

type ContinentMap = Map Continent [Country]

countriesByContinent :: FilePath -> IO ContinentMap
countriesByContinent countriesFile =
   readFile countriesFile >>=
   return . flip process Map.empty 
          . dropWhile (not . isPrefixOf "Continent") . lines

{--
A note about formatting:

deal with it.

That is all.
--}

process :: [String] -> ContinentMap -> ContinentMap
process [] ans = ans
process lines@(l:ines) acc = 
   let (row, rest) = processContinent lines
   in  process rest (maybe acc (flip (uncurry Map.insert) acc) row)

processContinent :: [String] -> (Maybe (Continent, [Country]), [String])
processContinent (conti:countrsAnd) =
   maybe (Nothing, []) (pc countrsAnd) (stripPrefix "Continent: " conti)

pc :: [String] -> Continent -> (Maybe (Continent, [Country]), [String])
pc resti counti = 
   let (countries, rest) = processCountries resti []
   in  (Just (counti, countries), rest)

processCountries :: [String] -> [Country] -> ([Country], [String])
processCountries [] ans = (ans, [])
processCountries (l:ines) acc | l == "" = (acc, ines)
                              | otherwise = processCountries ines (q:acc)
   where m = tail l
         (n, o) = break (== '-') m
         q = (if o == "" then id else init) n

{--
>>> countriesByContinent (workingDir ++ cbc)
fromList [("Africa",["Zimbabwe","Zambia",...], ...]

>>> let m = it
>>> Map.size m
7

>>> Map.map length m
fromList [("Africa",55),("Antarctica",0),("Asia",48),("Europe",49),
          ("North America",45),("Oceania",23),("South America",15)]
--}
