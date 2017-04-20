{-# LANGUAGE OverloadedStrings #-}

module Y2017.M04.D19.Solution where

import Data.Aeson
import Data.Function (on)
import Data.List (sortBy)

-- below imports available from 1HaskellADay git repository

import Data.Relation
import Wikidata.Query.Aeson
import Wikidata.Query.Builder
import Wikidata.Query.Endpoint

{--
Okay, today we're going to do the same thing as yesterday, but with a different
SPARQL query. Recall that we wanted eye-colors by frequency in wikidata.

Our original query was eyeColors in Y2017.M04.D14.Exercise

Create a value of SPARQL that gives an eye color query. Query wikidata and
return the results as a list of EyeColor values
--}

eyeColorQuery :: SPARQL
eyeColorQuery =
   let ec = Nod "eyeColor" True
       pers = Nod "person" False
       cnt = Nod "count" False
   in  Query False [Nd ec, Agg (Rel (Aggr COUNT pers) AS cnt)]
             [RDF (Trip (Rel pers (Name "wdt" "P1340") (Node ec)))]
             (Just (Sort GROUP [ec])) Nothing

data EyeColor = Eyes { color :: String, count :: String }
   deriving (Eq, Show)

instance FromJSON EyeColor where
   parseJSON o = Eyes <$> parseVal o "eyeColorLabel"
                      <*> parseVal o "count"

eyeColors :: SPARQL -> IO [EyeColor]
eyeColors queryVal = reifyWikiResults <$> sparql (queryBuilder queryVal)

{--
>>> eyeColors eyeColorQuery 
[Eyes {color = "violet", count = "1"},Eyes {color = "blue-green", count = "80"},
 Eyes {color = "brown", count = "735"},Eyes {color = "hazel", count = "178"},
 Eyes {color = "blue", count = "711"},Eyes {color = "green", count = "412"},
 Eyes {color = "red", count = "21"},Eyes {color = "black", count = "127"},
 Eyes {color = "dark brown", count = "132"},Eyes {color = "grey", count = "24"},
 Eyes {color = "amber", count = "22"},Eyes {color = "yellow", count = "30"},
 Eyes {color = "purple", count = "3"}]
--}

-- What is the most-mentioned eye-color? What is the least-mentioned one?

mostMentioned, leastMentioned :: [EyeColor] -> EyeColor
mostMentioned = last . sortBy (compare `on` (read :: String -> Int) . count)
leastMentioned = head . sortBy (compare `on` (read :: String -> Int) . count)

{--
>>> let ec = it
>>> mostMentioned ec
Eyes {color = "brown", count = "735"}
>>> leastMentioned ec
Eyes {color = "violet", count = "1"}
--}
