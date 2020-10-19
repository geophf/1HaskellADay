module Y2020.M10.D14.Exercise where

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

import Y2020.M10.D12.Exercise  -- Country-type

import Data.Map (Map)

workingDir :: FilePath
workingDir = "Y2020/M10/D14/"

cbc :: FilePath
cbc = "countries.txt"

type Continent = String

type ContinentMap = Map Continent [Country]

countriesByContinent :: FilePath -> IO ContinentMap
countriesByContinent countriesFile = undefined

{--
A note about formatting:

deal with it.

That is all.
--}
