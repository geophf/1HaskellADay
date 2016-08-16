{-# LANGUAGE ViewPatterns #-}

module Y2016.M08.D08.Solution where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Network.HTTP.Conduit (simpleHttp)
import Graph.JSON.Cypher.Read.Rows  -- available at 1HaskellADay git repository

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
readJSONRows = fmap (rows . fromJust . decode) . simpleHttp

-- *Y2016.M08.D08.Solution> readJSONRows url ~> json

-- with the above definition of readJSONRows, answer the below:

-- How many "row"-TableRow values are there?

rowRows :: [TableRow] -> Int
rowRows = length

-- *Y2016.M08.D08.Solution> rowRows json ~> 100

-- How many elements are in each inner array of each row for this JSON?

rowElements :: TableRow -> [Value]
rowElements (row -> Array arr) = 
   let (Array vect) = arr V.! 0 in V.toList vect

{-- 
*Y2016.M08.D08.Solution> (length &&& head) $ rowElements (head json) ~>
(5,Object (fromList [("screen_name",String "1HaskellADay"),...]))

We'll start to look at the structure(s) of these rows throughout the week.
--}
