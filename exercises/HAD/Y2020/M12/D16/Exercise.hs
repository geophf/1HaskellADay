{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D16.Exercise where

{--
Now that all the countries of the Five Powers Defense Arrangements have
capitals with (importantly) lats and longs, map this alliance, its countries,
and their air bases onto a global-viewer using KML/Keyhole Markup Language.
--}

import Data.Aeson

import qualified Data.Text as T

import Data.Aeson.WikiDatum (Name)
import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows (TableRow)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Y2020.M10.D30.Solution hiding (name)     -- for Alliance

fivePowers :: Endpoint -> Alliance -> IO String
fivePowers = undefined

-- ... you have to fetch that alliance, ... somehow, and the associated data.

-- which means we need to construct the Alliance from the graph-store:

data AllianceNames = AN Name [Name]
   deriving (Eq, Ord, Show)

-- The alliance names is the name of the alliance and its aliases.

allianceNamesOf :: Endpoint -> Name -> IO (Maybe AllianceNames)
allianceNamesOf url alliance = undefined
             
{--
... which means we need to parse an alliance from the returned TableRow values

... here is a sample row of an alliance returned from a query
--}
 
allianceNamesQuery :: Name -> Cypher
allianceNamesQuery alli =
   T.concat ["MATCH (a:Alliance { name: \"", alli, "\" }) RETURN a"]

{--
>>> :set -XOverloadedStrings 
>>> getGraphResponse url [allianceNamesQuery "Five Power Defence Arrangements"]
{"results":[{"columns":["a"],"data":[{"row":[
   {"name":"Five Power Defence Arrangements"}],
 "meta":[{"id":1215,"type":"node","deleted":false}]}]}],"errors":[]}

>>> let resp = it

so the actual row (n.b.: aliases is optional) is:
--}

fivePowerRow :: String
fivePowerRow = "[{\"name\":\"Five Power Defence Arrangements\"}]"

-- also note that a row is an array.

instance FromJSON AllianceNames where
   parseJSON = undefined

-- and from that you should get the (albeit underwhelming) Alliance name and
-- aliases. To get the alliance countries:
 
{--
>>> (decode $ BL.pack fivePowerRow) :: Maybe AllianceNames
Just (AN "Five Power Defence Arrangements" [])

... which means we can now do this:

>>> (RR.justRows resp) :: [TableRow [AllianceNames]]
[TR {row = [AN "Five Power Defence Arrangements" []]}]

... which is pretty dang powerful, if'n you're asking.

... of course, ... NONE of the Alliances are aliased, because, well, WAAAAY
back in October I left a TODO around parsing aliases from the file of the list
of Alliances, and did I ever get back to completing that TODO?

Noooooooo!

Le sigh.

An exercise for another day.

NEXT! We fetch the alliance's member countries:
--}

allianceCountriesQuery :: Name -> Cypher
allianceCountriesQuery alliance = 
   T.concat ["MATCH (:Alliance { name: \"", alliance, "\" })-[:MEMBER_OF]->",
             "(c:Country) RETURN c"]

{--
which returns:

>>> getGraphResponse url [allianceCountriesQuery "Five Power Defence Arrangements"]
{"results":[{"columns":["c"],"data":[
 {"row":[{"aliases":["UK"],"name":"United Kingdom",
          "qid":"http://www.wikidata.org/entity/Q145"}],
  "meta":[{"id":225,"type":"node","deleted":false}]},
 {"row":[{"name":"New Zealand","qid":"http://www.wikidata.org/entity/Q664"}],
  "meta":[{"id":145,"type":"node","deleted":false}]},
 {"row":[{"name":"Singapore"}],
  "meta":[{"id":184,"type":"node","deleted":false}]},
 {"row":[{"name":"Australia","qid":"http://www.wikidata.org/entity/Q408"}],
  "meta":[{"id":15,"type":"node","deleted":false}]},
 {"row":[{"name":"Malaysia","qid":"http://www.wikidata.org/entity/Q833"}],
  "meta":[{"id":120,"type":"node","deleted":false}]}]}],
 "errors":[]}

Here we see that qid is optional as well as aliases.
--}

data Country = Country { country :: Name, qid :: Maybe Name, aliases :: [Name] }
   deriving (Eq, Ord, Show)

instance FromJSON Country where
   parseJSON = undefined

theUK :: String
theUK = "{\"aliases\":[\"UK\"],\"name\":\"United Kingdom\","
        ++ "\"qid\":\"http://www.wikidata.org/entity/Q145\"}"

{--
>>> (decode (BL.pack theUK)) :: Maybe Country
Just (Country {country = "United Kingdom",  
               qid = Just "http://www.wikidata.org/entity/Q145",
               aliases = ["UK"]})
--}

singapore :: String
singapore = "{\"name\":\"Singapore\"}"

{--
>>> (decode (BL.pack singapore)) :: Maybe Country
Just (Country {country = "Singapore", qid = Nothing, aliases = []})
  
With this parser, we can do this:
  
>>> (RR.justRows resp) :: [TableRow [Country]]
[TR {row = [Country {country = "United Kingdom", 
                     qid = Just "http://www.wikidata.org/entity/Q145", 
                     aliases = ["UK"]}]},
 TR {row = [Country {country = "New Zealand", 
                     qid = Just "http://www.wikidata.org/entity/Q664",
                     aliases = []}]},
 TR {row = [Country {country = "Singapore",
                     qid = Nothing, 
                     aliases = []}]},
 TR {row = [Country {country = "Australia",
                     qid = Just "http://www.wikidata.org/entity/Q408",  
                     aliases = []}]},
 TR {row = [Country {country = "Malaysia",
                     qid = Just "http://www.wikidata.org/entity/Q833",
                     aliases = []}]}]

For a map of an Alliance: its countries, and their air bases, we need the
country's capital and the country's air bases. We will fetch those data,
tomorrow.
--}
