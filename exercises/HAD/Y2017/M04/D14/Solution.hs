module Y2017.M04.D14.Solution where

-- Below imports available via 1HaskellADay git repository

import Wikidata.Query.Endpoint

{--
So, what if we want to know, say, popular eye colors from wikidata, because
that's an import thing to store and to retrieve, apparently:
--}

eyeColors :: String
eyeColors = unlines ["SELECT ?eyeColorLabel (COUNT(?person) AS ?count)",
        "WHERE",
        "{",
        "?person wdt:P1340 ?eyeColor.",
        "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }",
        "}",
        "GROUP BY ?eyeColorLabel"]

-- using the above imports, send off the query to wikidata. What are the
-- results you get?

-- We'll look at decoding the results next week, as well as making other queries

-- You can look at other sample wikidata queries at:

sampleWikidataQueries :: FilePath
sampleWikidataQueries = "https://query.wikidata.org"

-- and select the 'Examples' button.

{--
>>> sparql eyeColors 
{
  "head" : {
    "vars" : [ "eyeColorLabel", "count" ]
  },
  "results" : {
    "bindings" : [ {
      "eyeColorLabel" : {
        "xml:lang" : "en",
        "type" : "literal",
        "value" : "violet"
      },
      "count" : {
        "datatype" : "http://www.w3.org/2001/XMLSchema#integer",
        "type" : "literal",
        "value" : "1"
      }
    },
...
      ] 
  }
}

Teaser:

Picture in your mind a data type, e.g.:

data Person { eyeColor :: String, count :: Int } deriving Show

that guides the creation of the SPARQL query and also decodes the JSON results
as Haskell values?

"Crazy," you say? Good, that means we're on the right track! MWA-HAHAHA!
--}
