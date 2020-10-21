module Y2020.M10.D20.Solution where

{--
Today, we're going to do a li'l bit-o-dis, a li'l bit o' dat, and do a case
study of merging airbase data (with their countries) with "wikidata" (and
I have "wikidata" in quotes for a reason, smh) of countries and continents.
--}

import Y2020.M10.D12.Solution    -- airbases
import Y2020.M10.D13.Solution    -- PROPERLY JSONified smh
import Y2020.M10.D14.Solution    -- Continents to countries
import Y2020.M10.D15.Solution    -- countries to Continents
import Y2020.M10.D16.Solution    -- ... as a graph

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

import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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
countries f = foldr (Set.insert . f) Set.empty . Map.toList

-- but that means we need to create a function that creates a function
-- to get our set of countries.

-- ... and does that mean `countries` reduces to a fold over a map?

-- ... and wait! Isn't fold comonadic? Isn't (w a -> a) comonadic? Hmmmmmm!

{--
>>> loadBases (Y2020.M10.D12.Solution.workingDir ++ file)
>>> let bs = it
>>> let mbs = byICAO bs
>>> let seta = countries (country . snd) mbs
>>> Set.size seta
108

>>> take 5 $ Set.toList seta
["Afghanistan","Albania","Algeria","Argentina","Australia"]

okay, so then:

>>> countriesByContinent (Y2020.M10.D14.Solution.workingDir ++ cbc)
>>> let conti = it
>>> let cm = countryMap conti
>>> let setb = countries fst cm
>>> Set.size setb
231

-- What's in set a that's not in set b? What's in set b that's not in set a?

>>> let notinb = Set.difference seta setb
>>> Set.size notinb 
43
>>> take 5 $ Set.toList notinb 
["Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina"]

>>> let notina = Set.difference setb seta
>>> Set.size notina
166

>>> take 5 $ Set.toList notina
["Albania (Shqip\235ria)","Andorra","Angola","Anguilla","Antigua and Barbuda"]

Write a 'repair'-function that replaces the graph countries with the airbase
countries.

So, the question becomes, how do we commute the setb's baroque names to 
seta's simpler names. Is there an easy transformation here?

For example "Albania" is a prefix of "Albania (Shqip\235ria)" ... is that
universally the case?

Let's ask Haskell.

>>> all (\c -> any (`isPrefixOf` c) setb) notinb
True

Haskell says 'yes!'

Wait.

>>> all (\c -> any (isPrefixOf c) setb) notinb
True

is also true? This concerns me.

Let's ask another question. notinb is 166, but seta is only 108 members.

Which seta members are in setb?

>>> let inter = Set.intersection seta setb
>>> take 5 $ Set.toList inter
["Afghanistan","Algeria","Argentina","Australia","Azerbaijan"]

>>> Set.size inter
65


So we know which countries do work. We also know that 40+ countries don't.

Which countries don't work?

>>> take 5 $ Set.toList noworki 
["Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina"]

>>> Set.size noworki 
43

Now let's ask that question again:

>>> all (flip any setb . flip isPrefixOf) noworki
False

... which ones don't work for this premise?

>>> Set.filter (flip any setb . flip isPrefixOf) noworki
fromList ["Dominican Republic"]

That's it? Just the Dominican Republic? Why?

After all, setb does have: "Dominican Republic (Republica Dominicana)"

Wait.

>>> Set.filter (flip any setb . isPrefixOf) noworki
fromList ["Albania","Austria","Belarus", ...]

>>> Set.size it
39


39 of the 43 work with this premise. Which ones don't?

>>> Set.filter (not . flip any setb . isPrefixOf) noworki
{"People's Republic of China","Saint Helena, Ascension and Tristan da Cunha",
 "Turkish Republic of Northern Cyprus","Yugoslavia"}

... poor, old Yugoslavia! :(

So, it looks like for 39 of the 43 mismatches we have a simple solution: replace
the wordy country name with the prefixed one.

For the other 4, we need to address on a case-by-case basis, I think, or,
grossly: hand-jam those puppies.

And here's my hand-jam (from reading up on these countries and then guessing: 

* Yugoslavia becomes Serbia, which we truncate (I know, three countries,
  but I have to pick one, so I picked one.)
* Saint Helena, Ascension and Tristan da Cunha become ... well, nothing, we
  don't have that fine a granularity to care about its airbase:

>>> filter ((== "Saint Helena, Ascension and Tristan da Cunha") . country . snd)
           (Map.toList mbs)
[("FHAW",
  Base {k = "http://www.wikidata.org/entity/Q1968916", 
        val = "RAF Ascension Island", icao = "FHAW", 
        country = "Saint Helena, Ascension and Tristan da Cunha", 
        pos = Point {lon = -14.393667, lat = -7.969667}})]
>>> length it
1

... eh, but what if this base is of strategic importance? Eh. I'll mark this
base as a British possession, because that's what it really is, isn't it.

* People's Republic of China becomes China (already truncated)
* Turkish Republic of Northern Cypress becomes Turkey, which we then truncate
  (the Turkish Republic of Northern Cypress is Turkey's and not Cypress'?)

Four problematic country-names, four solutions. And for the other 39 a solution,
as well.

So: first pass, we change those 4 countries in their airbases.
  : second pass, we truncate the wordy country names from setb to what's in
                 seta
  : third pass, we add the airbases, and they ... SHOULD all fit in somewhere.

Fingers crossed and watching the logs.
--}

firstPassFilter :: Country -> Country
firstPassFilter "Yugoslavia" = "Serbia"
firstPassFilter "Saint Helena, Ascension and Tristan da Cunha" = "United Kingdom"
firstPassFilter "People's Republic of China" = "China"
firstPassFilter "Turkish Republic of Northern Cypress" = "Turkey"

-- okay, that was easy.

{--
The second-pass filter needs to replace the key of the CountryMap with the
truncated version. So: we have to find the key with the truncated key, then
replace the key with the truncated key. Fortunately, Data.Map has so many
functions around update, it obviously thought of this scenario, too, right?

Le sigh.

Anyway.

First! We must find the key with the truncated key and return it (so we can
update it, ... later, ... by finding it again. idk) ... (with a maybe, where
'Nothing', in this case, means a perfect match (so you do nothing) but Just key
means that you'll need to replace that key with the truncated key, down the
road. Maybe.

findKey :: Country -> CountryMap -> Just Country
findKey key = 

Nah, that's crazy. Let's just, for a country, issue cypher if we have to 
truncate the name:
--}

secondPassFilter :: Country -> Set Country -> Maybe Cypher
secondPassFilter key = spf key . Set.lookupGE key

spf :: Country -> Maybe Country -> Maybe Cypher
spf a (Just b) | a == b = Nothing
               | otherwise = Just (updateKey a b)

updateKey :: Country -> Country -> Cypher
updateKey sh lo = "MATCH (c:Country { name: \"" ++ lo ++ "\" }) SET c.name=\""
               ++ sh ++ "\"; "

-- with the above filters in place, we have airbases with revised countries
-- and our graph with truncated countries. EVERYTHING SHOULD WORK(tm)

-- we'll find tha out when I do that, lol.

repair :: Map Country Country -> Graph -> Graph
repair = undefined

-- With the repaired-graph, upload the airbases to it, relating airbases to
-- countries in the graph.

uploadAirbases :: Map Icao AirBase -> Graph -> Graph
uploadAirbases = undefined

-- NOW answer a., b., and c.
