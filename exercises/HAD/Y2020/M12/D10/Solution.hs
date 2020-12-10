{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D10.Solution where

{--
Okay, I have a confession to make.

I cheated.

I said, yesterday, that there were unicode-issues and today we would address
them so we could realign the alliances from the aliased-countries to the
source ones.

Well, the only unicode issue was with one country, and how do we* refer to that
country? As its name in unicode? No. So, I renamed the country, then ran
yesterday's code, and it worked.

The code to fix the problem is as follows:

MATCH (c:Country) WHERE "Sao Tome and Principe" in c.aliases
SET c.aliases = ["São Tomé and Príncipe"], c.name = "Sao Tome and Principe" 
RETURN c

Run this fix, then run yesterday's solution.

Up to date now? Great!

Today, we're going to remove the Alliance relations from the aliased countries.

First, we need to get those now-redundant relations. But we already did that:
yesterday.
--}

import Control.Arrow ((&&&))

import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows (TableRow)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2020.M10.D28.Solution (Name)
import Y2020.M11.D17.Solution (CapAt, CapAt(CAPITAL))
import Y2020.M12.D01.Solution (Country, Country(Country))
import Y2020.M12.D09.Solution (Alias, AliasCountryMap)
import qualified Y2020.M12.D09.Solution as AA -- for aliased alliances

{--
>>> graphEndpoint 
...
>>> let url = it
>>> getGraphResponse url [AA.fetchAliasedAlliances]
...
>>> let aaq = it
>>> let aas = AA.aliasedAlliances $ map (AA.toAliasAlliance . RR.row) (RR.justRows aaq)
>>> Map.size aas
29

... and there they are. So we can delete them now, by writing more Haskell?

Well, that was the plan, but, hark! When is writing external functionality
to a database good, and when is it not? There are differing opinions on this,
but mine is that if you needuto do several (serial) things, like: moving 
relations from aliases to source countries, THEN deleting those original
relations, then yes, do those multiple steps programatically.

But.

If we're just deleting relations, let's ... just delete the relations.

MATCH (c:Country)<-[r:MEMBER_OF]-(d:Alliance)
WHERE not (c)-[:IN]->(:Continent)
DELETE r

Boom! Done! ... with pics! (see pic; this directory)

Okay, now that we did (didn't) today's Haskell problem (deleting aliased
alliance relations), let's do a new problem for today.

How many aliased alliances have capitals related to them?
--}

capitalQuery :: Cypher
capitalQuery =
   T.concat ["MATCH p=(c:Country)-[:CAPITAL]-(c1:Capital) ",
             "WHERE not (:Continent)--(c) ",
             "RETURN c.name as country, c1.name as capital"]

{--
returns the following:

country						capital
-----------------------------------------------------------
"People's Republic of China"			"Beijing"
"Saint Helena, Ascension and Tristan da Cunha"	"Jamestown"
"Yugoslavia"					"Belgrade"

Recall that we're ignoring the two countries of Saint Helena, Ascension and 
Tristan da Cunha, and Yugoslavia. (Yes, two countries. One country, the first
one, has three countries in its one country.)

We also see that the country China does not have a capital.* Let's move the
aliased capital to China, using the same approach as to yesterday.

* I say 'China' ... the other countries will fall out of the wash. Whatevs.
--}

data AliasedCapital = AC { alias :: Text, capital :: Text }
   deriving (Eq, Ord, Show)

toAliasedCapital :: [Text] -> AliasedCapital
toAliasedCapital = AC . head <*> last

{--
>>> getGraphResponse url [capitalQuery]
...
>>> let cq = it
>>> let ccs = map (toAliasedCapital . RR.row) $ RR.justRows cq
>>> ccs
[AC {alias = "People's Republic of China", capital = "Beijing"},
 AC {alias = "Saint Helena, Ascension and Tristan da Cunha", capital = "Jamestown"},
 AC {alias = "Yugoslavia", capital = "Belgrade"}]

Now, for these aliased countries, we need the source countries. The query
is different than yesterday's however.
--}

aliasCountryQuery :: Cypher
aliasCountryQuery =
   T.concat ["MATCH (c:Country)--(:Capital) ",
             "WHERE NOT (c)--(:Continent) ",
             "WITH c.name as alias ",
             "MATCH (c1:Country) ",
             "WHERE alias in c1.aliases ",
             "RETURN alias, c1.name as country"]

-- with the above query, we can get to our AliasCountryMap

{--
>>> getGraphResponse url [aliasCountryQuery]
{"results":[{"columns":["alias","country"],"data":[
     {"row":["People's Republic of China","China"],"meta":[null,null]}]}],
 "errors":[]}

... not much of a map, but hey! Like I said: the other countries dropped out.

>>> let acq = it
>>> let acm = AA.toAliasCountry $ map RR.row (RR.justRows acq)
>>> acm
fromList [("People's Republic of China",Country {country = "China"})]

As was yesterday, so today: we copy the relation of the aliased capital to
the source country. We can leverage the work we did in Y2020.M11.D17.Solution.
--}

data Capital = Capital Name
   deriving (Eq, Ord, Show)

instance Node Capital where
   asNode (Capital c) = constr "Capital" [("name", c)]

type RelCountryCapital = Relation Country CapAt Capital

relinkCapitals :: AliasCountryMap -> AliasedCapital -> Maybe RelCountryCapital
relinkCapitals acm alicap =
   mkRelink (Capital $ capital alicap) <$> Map.lookup (alias alicap) acm

mkRelink :: Capital -> Country -> RelCountryCapital
mkRelink cap country = Rel country CAPITAL cap

{--
>>> let rels = mapMaybe (relinkCapitals acm) ccs
>>> rels
[Rel (Country {country = "China"}) CAPITAL (Capital "Beijing")]

>>> cyphIt url rels
"{\"results\":[{\"columns\":[],\"data\":[]}],\"errors\":[]}"

Yay! Now we can delete this relation from PRC, ...

MATCH p=(c:Country)-[r:CAPITAL]-(d:Capital)
WHERE d.name = "Beijing"
AND NOT ()-->(c)
DELETE r

... and also delete singleton nodes with:
--}

singletonNodesQuery :: Cypher
singletonNodesQuery =
   "MATCH (c) WHERE NOT ()-->(c) AND NOT (c)-->() RETURN c.name"

{--
>>> getGraphResponse url [singletonNodesQuery]
>>> let qer = it
>>> let strs = (RR.justRows qer) :: [TableRow [String]]
>>> let singies = Set.fromList (map (head . RR.row) strs)
>>> Set.size singies
30

... (30 country (aliases) returned)

Are all these nodes found somewhere? Let's see:
--}

countriesWithAliasesQuery :: Cypher
countriesWithAliasesQuery =
   T.concat ["MATCH (c) ",
             "WHERE NOT ()-->(c) ",
             "AND NOT (c)-->() ",
             "WITH c.name AS alias ",
             "MATCH (q:Country) WHERE alias IN q.aliases ",
             "RETURN alias, q.name AS country"]

{--
>>> getGraphResponse url [countriesWithAliasesQuery]
...
>>> let mac = it
>>> let macr = (RR.justRows mac) :: [TableRow [String]]
>>> let macm = Map.fromList (map ((head &&& last) . RR.row) macr)
>>> Map.size macm
28

>>> Set.difference singies (Map.keysSet macm) 
fromList ["Ansarullah","Hezbollah"]

Which is fine, because those 'countries' are not countries.

So now we can delete the singleton nodes with:

MATCH (c) WHERE NOT ()-->(c) AND NOT (c)-->() DELETE c

When we execute that Cypher against the graph store, our query:

>>> getGraphResponse url [singletonNodesQuery]
"{\"results\":[{\"columns\":[\"c.name\"],\"data\":[]}],\"errors\":[]}"

... now returns the empty set of nodes.

Mission accomplished.
--}

{-- BONUS -------------------------------------------------------

This all begs the question of our graph:

Which countries-in-alliances have capitals? And which ones do not?

With the countries that do not have capitals, can we populate that information?

How?

The query to see which countries-in-alliances that do not have capitals is:
--} 

noCAPSquery :: Cypher
noCAPSquery =
   T.concat ["MATCH (a:Alliance)--(c:Country) ",
             "WHERE not (c)--(:Capital) ", 
             "RETURN DISTINCT c.name"]

{--
Write a Haskell function that extracts that information from the graph
data-store.
--}

capitalless :: Endpoint -> Cypher -> IO (Set Name)
capitalless url cyph =
   getGraphResponse url [cyph] >>= \resp ->
   let ncq = (RR.justRows resp) :: [TableRow [Text]] in
   return . Set.fromList $ map (head . RR.row) ncq

{--
>>> > capitalless url noCAPSquery 
{"Albania","Angola","Antigua and Barbuda","Armenia","Austria","Bangladesh",
 "Barbados","Belize","Benin","Botswana","Brazil","Brunei","Burkina Faso",
 "Burundi","Cambodia","Cameroon","Cape Verde","Central African Republic",
 "Colombia","Comoros","Costa Rica","Cuba","Cyprus","Dominica","East Timor",
 "Equatorial Guinea","Gabon","Ghana","Grenada","Guatemala","Guine","Guinea",
 "Guinea-Bissau","Guyana","Haiti","Iceland","Italy","Jamaica","Japan","Kenya",
 "Laos","Lesotho","Liberia","Luxembourg","Malawi","Maldives","Mali","Malta",
 "Mauritania","Mauritius","Moldova","Mongolia","Montenegro","Montserrat",
 "Morocco","Mozambique","Namibia","Netherlands","Nicaragua","Nigeria",
 "Palestine","Paraguay","Republic of Ireland","Republic of the Congo","Rwanda",
 "Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines",
 "Senegal","Seychelles","Sierra Leone","Singapore","Slovenia","Somalia",
 "South Sudan","Spain","Suriname","Swaziland (Eswatini)","Tajikistan",
 "The Bahamas","The Gambia","Togo","Trinidad and Tobago","Uganda",
 "Western Sahara","Yemen","Zambia"]

>>> Set.size it
87

Oh, dear me!

Also. I just realized something that saddened me. Y2020.M11.D17.Solution
had the lat/long data of capitals, but I did not load that information
to the graph store, but, instead, dropped the lat/longs on the floor.

Guess what we're doing tomorrow? le sigh.
--}
