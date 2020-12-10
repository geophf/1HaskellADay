{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D10.Exercise where

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

import qualified Data.Map as Map

import Data.Set (Set)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows (TableRow)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2020.M10.D28.Solution (Name)
import Y2020.M11.D17.Solution (CapAt)

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
toAliasedCapital = undefined

{--          
>>> graphEndpoint 
...
>>> let url = it
>>> getGraphResponse url [capitalQuery]
...
>>> let cq = it                                 
>>> map (toAliasedCapital . RR.row) $ RR.justRows cq
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
   asNode = undefined

type RelCountryCapital = Relation Country CapAt Capital

relinkCapitals :: AliasCountryMap -> Alias -> Capital -> [RelCountryCapital]
relinkCapitals acm ali cap = undefined

mkRelink :: Capital -> Country -> RelCountryCapital
mkRelink = undefined

{-- BONUS -------------------------------------------------------

This all begs the question of our graph:

Which countries-in-alliances have capitals? And which ones do not?

With the countries that do not have capitals, can we populate that information?

How?

The query to see which countries-in-alliances that do not have capitals is:

MATCH p=(a:Alliance)--(c:Country)
WHERE not (c)--(:Capital)
RETURN DISTINCT c.name

Write a Haskell function that extracts that information from the graph
data-store.
--}

capitalless :: Endpoint -> Cypher -> IO (Set Name)
capitalless = undefined

{--
Also. I just realized something that saddened me. Y2020.M11.D17.Solution
had the lat/long data of capitals, but I did not load that information
to the graph store, but, instead, dropped the lat/longs on the floor.

Guess what we're doing tomorrow? le sigh.
--}
