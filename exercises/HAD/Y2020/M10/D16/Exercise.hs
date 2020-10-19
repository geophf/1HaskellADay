module Y2020.M10.D16.Exercise where


{--
`sequence` is really cool: a neat trick.

The problem is that we have to use it in the first place. The question becomes
if the data structure you are using doesn't give you what you need, then you
need to move to a better data model. Data.Map isn't bidirecional, and that's
fine in many cases, but isn't in this case where we needed to key from the
country to the continent, and not the waythe original structure was built.

So. What data structure?

Hello, Graph.
--}

import Graph.Query

-- or, if you prefer:

import Data.Graph

import Y2020.M10.D12.Exercise -- Country-type
import Y2020.M10.D14.Exercise

{--
Take the countries and continents from before and structure these data in a
graph.

In which continent is Andorra?
What are the countries in ... 'Oceania'?
What are all the continents? What is each continent's countries?
--}

continentOf :: Graph -> Country -> Continent
continentOf = undefined

countriesOf :: Graph -> Continent -> [Country]
countriesOf = undefined

continentsCountries :: Graph -> [(Continent, [Country])]
continentsCountries = undefined
