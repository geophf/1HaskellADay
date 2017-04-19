module Y2017.M04.D19.Exercise where

import Data.Aeson

-- below imports available from 1HaskellADay git repository

import Wikidata.Query.Aeson
import Wikidata.Query.Endpoint

import Y2017.M04.D18.Exercise

{--
Okay, today we're going to do the same thing as yesterday, but with a different
SPARQL query. Recall that we wanted eye-colors by frequency in wikidata.

Our original query was eyeColors in Y2017.M04.D14.Exercise

Create a value of SPARL that gives an eye color query. Query wikidata and
return the results as a list of EyeColor values
--}

eyeColorQuery :: SPARQL
eyeColorQuery = undefined

data EyeColor = Eyes { color :: String, count :: Int }
   deriving Eq

instance FromJSON EyeColor where
   parseJSON = undefined

eyeColors :: SPARQL -> IO [EyeColor]
eyeColors queryVal = undefined

-- What is the most-mentioned eye-color? What is the least-mentioned one?

mostMentioned, leastMentioned :: [EyeColor] -> EyeColor
mostMentioned = undefined
leastMentioned = undefined
