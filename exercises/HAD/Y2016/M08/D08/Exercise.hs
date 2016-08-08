module Y2016.M08.D08.Exercise where

import Data.Aeson
import qualified Data.Vector as V
import Network.HTTP.Conduit (simpleHttp)  -- conduit is available via cabal
import Graph.JSON.Cypher    -- available at 1HaskellADay git repository

{--
This is interesting.

At this directory is result00100.json, also at the URL:
--}

url :: FilePath
url = "https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M08/D08/result00100.json"

{--
Let's, today, just read in the rows of JSON and dig into just a bit of the
information contained in these rows.
--}

readJSONRows :: FilePath -> IO [TableRow]
readJSONRows url = undefined

-- Hint: look at Graph.JSON.Cypher.justRows ... maybe that will help?
-- Hint: You can't use it directly with this JSON, but it is a starting point
-- of inquiry!

-- with the above definition of readJSONRows, answer the below:

-- How many "row"-TableRow values are there?

rowRows :: [TableRow] -> Int
rowRows = undefined

-- Hint: bit of a trick question.

-- How many elements does each row have? 
-- Simple: each row has 1 array of elements

-- How many elements are in each inner array of each row for this JSON?
-- What are the types of these elements? List the values of one of the rows.

rowElements :: TableRow -> [Value]
rowElements = undefined

-- Hint: read up on Data.Aeson.Types and Data.Vector

-- We'll start to look at the structure(s) of these rows throughout the week.
