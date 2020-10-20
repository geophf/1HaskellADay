module Y2020.M10.D20.Exercise where

{--
Today, we're going to do a li'l bit-o-dis, a li'l bit o' dat, and do a case
study of merging airbase data (with their countries) with "wikidata" (and
I have "wikidata" in quotes for a reason, smh) of countries and continents.
--}

import Y2020.M10.D12.Exercise    -- airbases
import Y2020.M10.D13.Exercise    -- PROPERLY JSONified smh
import Y2020.M10.D14.Exercise    -- Continents to countries
import Y2020.M10.D15.Exercise    -- countries to Continents
import Y2020.M10.D16.Exercise    -- ... as a graph

{--
The Problem.

We have Continents and countries (sometimes: highly-commented countries) in
a graph, now we want to upload the airbases, with country-information, to that
graph.

Do that.

1. Which airbases do NOT upload to the graph? Why? Hint: country-name mismatch.
2. Which country names do not match up between airbases and the graph? Update
   the graph to accommodate the airbases' countries' names.
3. Reupload the airbase-countries to the graph. Answer the following questions.

a. Which country has the most airbases?
b. Which continent has the most airbases?
c. Which airbase has the most continents?

Haha, just kidding about c. above

so: 
d. What are the countries of all the airbases?
e. what are the countries of the graph?
--}

import Data.Map (Map)
import Data.Set (Set)
import Data.Graph

{--
countries :: Map Icao AirBase -> Set Country
countries :: CountryMap -> Set Country

You see the problem? We want to have a map of:

Map ThingWeDon'tMuchCareAbout ThingThatHasACountry

OR

Map Country DifferentThingWeDon'tMuchCareAbout

to 

Set Country

We ... COULD have separate functions to do this? But then, what do we name
these separate functions that do the same kind of work?

Or we COULD pass in a map-type of either of the above, AND also pass in a
discriminating function to the function:
--}

countries :: ((a,b) -> Country) -> Map a b -> Set Country
countries = undefined

-- but that means we need to create a function that creates a function
-- to get our set of countries.

-- ... and does that mean `countries` reduces to a fold over a map?

-- ... and wait! Isn't fold comonadic? Isn't (w a -> a) comonadic? Hmmmmmm!

-- What's in set a that's not in set b? What's in set b that's not in set a?

-- Write a 'repair'-function that replaces the graph countries with the airbase
-- countries.

repair :: Map Country Country -> Graph -> Graph
repair = undefined

-- With the repaired-graph, upload the airbases to it, relating airbases to
-- countries in the graph.

uploadAirbases :: Map Icao AirBase -> Graph -> Graph
uploadAirbases = undefined

-- NOW answer a., b., and c.
