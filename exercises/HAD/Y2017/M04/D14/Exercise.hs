module Y2017.M04.D14.Exercise where

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
