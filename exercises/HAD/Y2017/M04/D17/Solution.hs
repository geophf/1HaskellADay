{-# LANGUAGE OverloadedStrings #-}

module Y2017.M04.D17.Solution where

-- A lot of scary imports below that will be subsumed by helper modules
-- when it comes to processing Wikidata results returned

import Data.Aeson       -- available from cabal install aeson
import Data.Aeson.Types
import Control.Arrow ((&&&))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (toList)
import Data.HashMap.Lazy ((!))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Vector (Vector)

-- below imports available via 1HaskellADay git repository

import Wikidata.Query.Endpoint

{--
On Friday, we saw a query for the frequency of eye color mentions in wikidata.

That was nice. Today we're going to ask for the United States capitols, and
then parse the returned JSON into Haskell values.
--}

stateCapitalsQuery :: String
stateCapitalsQuery =
    unlines ["SELECT DISTINCT ?state ?stateLabel ?capital ?capitalLabel", -- ?coordinates"
    "WHERE { ?state wdt:P31 wd:Q35657.",
             "?state wdt:P36 ?capital.",
             -- "?capital wdt:P625 ?coordinates.",
             "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" }",
          "}",
    "ORDER BY ?stateLabel"]

-- FYI: we can show this on a map by uncommenting the coordinates information
-- and adding #defaultView:Map pragma

-- Query the wikidata REST endpoint and get this information back (as JSON)
-- Parse the results returned into the following Haskell structure:

data StateCapital = StateCap { state, capital :: String }
  deriving (Eq, Ord, Show)

instance FromJSON StateCapital where
   parseJSON o = StateCap <$> parseVal o "stateLabel"
                          <*> parseVal o "capitalLabel"

parseVal :: FromJSON a => Value -> Text -> Parser a
parseVal = parseDepth "value"

parseDepth :: FromJSON a => Text -> Value -> Text -> Parser a
parseDepth sub (Object o) key = o .: key >>= (.: sub)

stateCapitals :: String -> IO [StateCapital]
stateCapitals query = reifyWikiResults <$> sparql query

-- ... or, given any input Wikidata results as a ByteString, returns [a]

reifyWikiResults :: FromJSON a => ByteString -> [a]
reifyWikiResults = fromKinda . mapM fromJSON . resultSet

fromKinda :: Result (Vector a) -> [a]
fromKinda (Error e) = []
fromKinda (Success x) = toList x

resultSet :: ByteString -> Array
resultSet bytes =
    let (Just ress) = (decode bytes :: Maybe Object)
        o = ress ! "results"
        (Object binds) = o
        anss = binds ! "bindings"
        (Array arr) = anss in
   arr

-- Holy eff I hate this JSON interpretation.

-- now that we have a result-set we can work with it to get a set of state
-- capitals ... all this will be going into the Wikidata parsing modules

-- What is the Capital of Louisiana? What is the Capital of New York?

caps :: [StateCapital] -> Map String String
caps = Map.fromList . map (state &&& capital)

{--
>>> stateCapitals stateCapitalsQuery ~> ans
>>> usa = caps ans
>>> usa Map.! "Louisiana"
"Baton Rouge"
>>> usa Map.! "New York"
"Albany"
--}

-- We'll be looking at providing hints to the data-type to allow us to 'ask'
-- it to construct its own wikidata query for us.

-- Does this mean we'll be hand-rolling our own RDF in Haskell? ... *shudder*
