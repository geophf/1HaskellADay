module Y2017.M04.D17.Exercise where

import Data.Aeson       -- available from cabal install aeson

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
   parseJSON json = undefined

stateCapitals :: FilePath -> IO [StateCapital]
stateCapitals wikidataEndpoint = undefined

-- What is the Capital of Louisiana? What is the Capital of New York?

-- We'll be looking at providing hints to the data-type to allow us to 'ask'
-- it to construct its own wikidata query for us.

-- Does this mean we'll be hand-rolling our own RDF in Haskell? ... *shudder*
