module Y2020.M10.D15.Solution where

import Y2020.M10.D12.Solution          -- for Country-type
import Y2020.M10.D14.Solution

{--
...

-- sigh.

But, of course, now that I have ContinentMap, I don't want that.

I want to know the continent of a given country, and I don't want to do
a reverse lookup to find that continent. What I'm really after is this:
--}

import Data.Tuple (swap)

import Data.Map (Map)
import qualified Data.Map as Map

type CountryMap = Map Country Continent

-- But, given ContinentMap, it's 'easy' to derive CountryMap

-- Today's Haskell problem is for you to do so.

countryMap :: ContinentMap -> CountryMap
countryMap = Map.fromList . map swap . concat . map sequence . Map.toList

{--
What is the size of the ContinentMap?

>>> countriesByContinent (workingDir ++ cbc)
>>> let m = it
>>> Map.size m
7

What is the size of the CountryMap?

>>> let cm = countryMap m
>>> Map.size cm
231

>>> take 5 $ Map.toList cm
[("Afghanistan","Asia"),("Albania (Shqip\235ria)","Europe"),("Algeria","Africa"),
 ("Andorra","Europe"),("Angola","Africa")]
--}
