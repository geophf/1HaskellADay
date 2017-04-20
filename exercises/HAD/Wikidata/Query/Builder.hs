module Wikidata.Query.Builder where

-- We present a typeful representation of simple Wikidata SPARQL queries

import Prelude hiding (GT, EQ, LT)
import Data.List (intercalate)

-- below imports available from 1HaskellADay git repository

import Control.Logic.Frege ((-|))
import Data.Relation hiding (Node)

import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

{--
We present the types and a function that constructs a SPARQL query, given hints
by some constrained type, call it SPARQL or call it NATION because everybody
love Donnie Darko (see the movie if you don't get the reference).

(see the movie if you DO get the reference.)
--}

data SPARQL = Query {
   distinct :: Bool,
   names :: [Aggregator],
   clauses :: [WikiClause],
   sorting :: Maybe Sort,
   limit :: Maybe Int }
   deriving (Eq, Show)

data Limit = Lmt Int

instance Show Limit where
   show (Lmt lmt) = "LIMIT " ++ show lmt

mbshow :: Show a => Maybe a -> String
mbshow Nothing = ""
mbshow (Just a) = ' ':show a

spacing :: Aggregator -> String
spacing (Nd nd@(Nod _ lbl)) = ' ': show nd ++ (lbl -| (' ':show nd ++ "Label"))
spacing (Agg rel)  = ' ':showRel rel

service :: String -> String
service lang = "SERVICE wikibase:label { bd:serviceParam wikibase:language "
   ++ show lang ++ " }"

-- where our nodes and properties are:

data Node = Nod { name :: String, labeled :: Bool }
   deriving Eq

instance Show Node where show = ('?':) . name

data Subst = AS
   deriving (Eq, Show)

showRel :: (Show a, Show b, Show c) => Relation a b c -> String
showRel (Rel a b c) = show a ++ (' ':show b ++ (' ':show c))

data Aggregator = Nd Node | Agg (Relation Aggregate Subst Node)
   deriving Eq

instance Show Aggregator where
   show (Nd nd) = show nd
   show (Agg rel) = showRel rel

-- problem 1: what is the show instance for aggregators that show the
-- SELECT values in a SPARQL query?

data Aggregate = Aggr AggrFn Node
   deriving Eq

instance Show Aggregate where
   show (Aggr af nd) = show af ++ ('(':show nd ++ ")")

data AggrFn = COUNT | SAMPLE | MAX | MIN | YEAR | MONTH | DAY | LANG
   deriving (Eq, Ord, Show)

data Sort = Sort Sorter [Node]
   deriving Eq

instance Show Sort where
   show (Sort srt nds) = show srt ++ " BY" ++ concatMap spacelabel nds

spacelabel :: Node -> String
spacelabel nd = ' ':show nd ++ (labeled nd -| "Label")

spaceout :: Show a => [a] -> String
spaceout list = ' ':intercalate " " (map show list)

data Sorter = GROUP | ORDER
   deriving (Eq, Ord, Show)

-- problem 2: What is the show instance for Sort for a wikidata query?

data WikiClause = RDF RDFTriple
                | OPTIONAL RDFTriple
                | FILTER (Relation Aggregator BoolProp Val)
                | BIND (Relation String Subst Node)
   deriving Eq

instance Show WikiClause where
   show wcl = doshow wcl ++ "."

doshow :: WikiClause -> String
doshow (RDF trip) = show trip
doshow (OPTIONAL opt) = "OPTIONAL ( " ++ show opt ++ " )"
doshow (FILTER rel) = "FILTER ( " ++ showRel rel ++ " )"
doshow (BIND (Rel binding sb nd)) =
   "BIND ( " ++ binding ++ (' ':show sb ++ (' ':show nd)) ++ " )"

-- problem 3: what is the show instance of a (simple) RDF triple

data RDFTriple = Trip (Relation Node NodeOrName NodeOrName)
   deriving Eq

instance Show RDFTriple where
   show (Trip rel) = showRel rel

-- problem 4: what is the show instance of a NodeOrName value?

data NodeOrName = Node Node | Name { namespace, qname :: String }
   deriving Eq

instance Show NodeOrName where
   show (Node nd) = show nd
   show (Name ns q) = ns ++ (':':q)

-- problem 5: what is the show instance of a BoolProp?

data BoolProp = LT | LE | EQ | GE | GT
   deriving (Eq, Ord)

instance Show BoolProp where
   show LT = "<"
   show LE = "<="
   show EQ = "=="
   show GE = "=>"
   show GT = ">"

data Val = S String | N Integer
   deriving Eq

-- problem 6: what is the show instance of a Val value?

instance Show Val where
   show (S str) = show str -- intentional (we need to show the quotes)
   show (N n)   = show n   -- intentional here, too

{-- USAGE -----------------------------------------------------------------

-- a. Create a StateCapital SPARQL value

stateCapitalSPARQL :: SPARQL
stateCapitalSPARQL =
   let st = Nod "state" True
       cp = Nod "capital" True in
   Query True (map Nd [st, cp])
        [RDF (Trip (Rel st (Name "wdt" "P31") (Name "wd" "Q35657"))),
         RDF (Trip (Rel st (Name "wdt" "P36") (Node cp)))]
         (Just (Sort ORDER [st])) Nothing
         

-- b. Write a query-generating function for a SPARQL value

queryBuilder :: SPARQL -> String
queryBuilder (Query dis ns cl sort lmt) =
      "SELECT" ++ (dis -| " DISTINCT") ++ concatMap spacing ns
               ++ " WHERE {" ++ spaceout cl
               ++ service "en" ++ " }"
               ++ mbshow sort ++ mbshow (Lmt <$> lmt)

-- c. Build the query. How does it compare to the original stateCapitalsQuery?

>>> queryBuilder stateCapitalSPARQL 
"SELECT DISTINCT ?state ?stateLabel ?capital ?capitalLabel WHERE { ?state 
wdt:P31 wd:Q35657. ?state wdt:P36 ?capital.SERVICE wikibase:label { 
bd:serviceParam wikibase:language \"en\" } } ORDER BY ?stateLabel"

Lookin' good!

-- d. Query the endpoint, reifying the results to StateCapital values. What 
--    result did you get?

stateCapitals :: SPARQL -> IO [StateCapital]
stateCapitals queryVal = reifyWikiResults <$> sparql (queryBuilder queryVal)

>>> stateCapitals stateCapitalSPARQL >>= mapM_ print
StateCap {state = "Alabama", capital = "Montgomery"}
StateCap {state = "Alaska", capital = "Juneau"}
StateCap {state = "Arizona", capital = "Phoenix"}
StateCap {state = "Arkansas", capital = "Little Rock"}
StateCap {state = "California", capital = "Sacramento"}
StateCap {state = "Colorado", capital = "Denver"}
StateCap {state = "Connecticut", capital = "Hartford"}
StateCap {state = "Delaware", capital = "Dover"}
StateCap {state = "Florida", capital = "Tallahassee"}
StateCap {state = "Georgia", capital = "Atlanta"}
StateCap {state = "Hawaii", capital = "Honolulu"}
StateCap {state = "Idaho", capital = "Boise"}
StateCap {state = "Illinois", capital = "Springfield"}
StateCap {state = "Indiana", capital = "Indianapolis"}
StateCap {state = "Iowa", capital = "Des Moines"}
StateCap {state = "Kansas", capital = "Topeka"}
StateCap {state = "Kentucky", capital = "Frankfort"}
StateCap {state = "Louisiana", capital = "Baton Rouge"}
StateCap {state = "Maine", capital = "Augusta"}
StateCap {state = "Maryland", capital = "Annapolis"}
StateCap {state = "Massachusetts", capital = "Boston"}
StateCap {state = "Michigan", capital = "Lansing"}
StateCap {state = "Minnesota", capital = "Saint Paul"}
StateCap {state = "Mississippi", capital = "Jackson"}
StateCap {state = "Missouri", capital = "Jefferson City"}
StateCap {state = "Montana", capital = "Helena"}
StateCap {state = "Nebraska", capital = "Lincoln"}
StateCap {state = "Nevada", capital = "Carson City"}
StateCap {state = "New Hampshire", capital = "Concord"}
StateCap {state = "New Jersey", capital = "Trenton"}
StateCap {state = "New Mexico", capital = "Santa Fe"}
StateCap {state = "New York", capital = "Albany"}
StateCap {state = "North Carolina", capital = "Raleigh"}
StateCap {state = "North Dakota", capital = "Bismarck"}
StateCap {state = "Ohio", capital = "Columbus"}
StateCap {state = "Oklahoma", capital = "Oklahoma City"}
StateCap {state = "Oregon", capital = "Salem"}
StateCap {state = "Pennsylvania", capital = "Harrisburg"}
StateCap {state = "Rhode Island", capital = "Providence"}
StateCap {state = "South Carolina", capital = "Columbia"}
StateCap {state = "South Dakota", capital = "Pierre"}
StateCap {state = "Tennessee", capital = "Nashville"}
StateCap {state = "Texas", capital = "Austin"}
StateCap {state = "Utah", capital = "Salt Lake City"}
StateCap {state = "Vermont", capital = "Montpelier"}
StateCap {state = "Virginia", capital = "Richmond"}
StateCap {state = "Washington", capital = "Olympia"}
StateCap {state = "West Virginia", capital = "Charleston"}
StateCap {state = "Wisconsin", capital = "Madison"}
StateCap {state = "Wyoming", capital = "Cheyenne"}

WOOT!

So, maybe, we still haven't found what we're looking for ... or maybe we have.
--}
