module Y2017.M04.D18.Exercise where

-- below imports available from 1HaskellADay git repository

import Data.Relation hiding (Node)

import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

import Y2017.M04.D17.Exercise (stateCapitalsQuery, StateCapital)

{--
So, yesterday, we parsed out State Capitals of the USA. YAY!

But the query was written by hand. BOO!

So, let's write the query by ... UNHAND! YAY!

Today we'll be writing a function that constructs a SPARQL query, given hints
by some constrained type, call it SPARQL or call it NATION because everybody
love Donnie Darko (see the movie if you don't get the reference).

(see the movie if you DO get the reference.)
--}

data SPARQL = Query {
   distinct :: Bool,
   names :: [Aggregator],
   clauses :: [WikiClause],
   sorting :: Sort,
   limit :: Maybe Int }
   deriving Eq

-- where our nodes and properties are:

data Node = Nod { name :: String, labeled :: Bool }
   deriving Eq

instance Show Node where show = ('?':) . name

data Subst = AS
   deriving (Eq, Show)

data Aggregator = Nd Node | Agg (Relation Aggregate Subst Node)
   deriving Eq

instance Show Aggregator where
   show = undefined

-- problem 1: what is the show instance for aggregators that show the
-- SELECT values in a SPARQL query?

data Aggregate = AggrFn Node
   deriving Eq

data AggrFn = COUNT | SAMPLE | MAX | MIN | YEAR | MONTH | DAY | LANG
   deriving (Eq, Ord, Show)

data Sort = Sorter [Node]
   deriving Eq

instance Show Sort where
   show = undefined

data Sorter = GROUP | ORDER
   deriving (Eq, Ord, Show)

-- problem 2: What is the show instance for Sort for a wikidata query?

data WikiClause = RDF RDFTriple
                | OPTIONAL RDFTriple
                | FILTER (Relation Aggregator BoolProp Val)
                | BIND (Relation String Subst Node)
   deriving Eq

instance Show WikiClause where
   show = undefined

-- problem 3: what is the show instance of a (simple) RDF triple

data RDFTriple = Trip (Relation Node NodeOrName NodeOrName)
   deriving Eq

instance Show RDFTriple where
   show = undefined

-- problem 4: what is the show instance of a NodeOrName value?

data NodeOrName = Node Node | Name { namespace, qname :: String }
   deriving Eq

instance Show NodeOrName where
   show = undefined

-- problem 5: what is the show instance of a BoolProp?

data BoolProp = LT | LE | EQ | GE | GT
   deriving (Eq, Ord)

instance Show BoolProp where
   show = undefined

data Val = S String | N Integer
   deriving Eq

-- problem 6: what is the show instance of a Val value?

instance Show Val where
   show = undefined

-- SO! -----------------------------------------------------------------

-- a. Create a StateCapital SPARQL value

stateCapitalSPARQL :: SPARQL
stateCapitalSPARQL = undefined

-- b. Write a query-generating function for a SPARQL value

queryBuilder :: SPARQL -> String
queryBuilder val = undefined

-- c. Build the query. How does it compare to the original stateCapitalsQuery?

-- d. Query the endpoint, reifying the results to StateCapital values. What 
--    result did you get?

stateCapitals :: SPARQL -> IO [StateCapital]
stateCapitals queryVal = undefined

{-- Tomorrow we'll take on the eye-color query.

Also, all the above is a lot, but we have an eye toward these kinds of queries:

#defaultView:Map
SELECT DISTINCT ?borough ?boroughLabel (SAMPLE(?location) AS ?location)
                (MAX(?population) AS ?population) (SAMPLE(?layer) AS ?layer)
WHERE
{
  ?borough wdt:P31/wdt:P279* wd:Q13410522;
       # wdt:P131 wd:Q797;
        wdt:P625 ?location;
        wdt:P1082 ?population.
  FILTER(?population >= 50).
  BIND(
    IF(?population < 1000, "<1k",
    IF(?population < 2000, "1k-2k",
    IF(?population < 5000, "2k-5k",
    IF(?population < 10000, "5k-10k",
    IF(?population < 20000, "10k-20k",
    ">20k")))))
    AS ?layer).
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}
GROUP BY ?borough ?boroughLabel

So, maybe, we still haven't found what we're looking for ... or maybe we have.
--}
