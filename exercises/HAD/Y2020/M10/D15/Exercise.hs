module Y2020.M10.D15.Exercise where

import Y2020.M10.D14.Exercise

{--
...

-- sigh.

But, of course, now that I have ContinentMap, I don't want that.

I want to know the continent of a given country, and I don't want to do
a reverse lookup to find that continent. What I'm really after is this:
--}

import Data.Map (Map)

type CountryMap = Map Country Continent

-- But, given ContinentMap, it's 'easy' to derive CountryMap

-- Today's Haskell problem is for you to do so.

countryMap :: ContinentMap -> CountryMap
countryMap = undefined

{--
What is the size of the ContinentMap?
What is the size of the CountryMap?
--}
