{-# LANGUAGE OverloadedStrings #-}

module Graph.JSON.Cypher.Read.Rows where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)

-- Reads in rows of JSON from a Cypher query result

data GraphResults = GR { results :: [TabledResults], errors :: [Errors] }
   deriving Show

instance FromJSON GraphResults where
   parseJSON (Object v) = GR <$> v .: "results" <*> v .: "errors"

data TabledResults = Table { columns :: [String], rows :: [TableRow] }
   deriving Show

instance FromJSON TabledResults where
   parseJSON (Object v) = Table <$> v .: "columns" <*> v .: "data"

type Errors = Object

data TableRow = TR { row :: Value } deriving Show

instance FromJSON TableRow where
   parseJSON (Object v) = TR <$> v .: "row"

{--
*Main> getGraphResponse (endpoint ++ ('/': transaction)) queryTop5shows ~> ans
see Graph.Query                   -- http://lpaste.net/6813513488191717376
for getGraphResponse
*Main> let json = BL.pack ans
*Main> let (Just (GR res errs)) = (decode json) :: Maybe GraphResults 
*Main> length res ~> 1
*Main> columns (head res) ~> ["Security","Date"]
*Main> last . rows $ head res ~> 
TR {row = Array [String "ZSPH",
                 Array [String "2015-09-10",String "2015-10-07",...]]}
... the above gets you parsed to any Cypher query to graph DaaS
Now, to translate a TableRow into your structure, you need simply translate
that Value to your structure.
e.g.:
row2Top5s :: TableRow -> Top5sAppearance
row2Top5s = parseTop5sShow . init . tail . BL.unpack . encode . row 
Then, given that, you can take the string (the untyped-JSON response) and 
convert that to a data set you can work with directly, e.g.:
--}

type QueryResult = String

{--
top5shows :: QueryResult -> [Top5sAppearance]
top5shows = map row2Top5s . rows . head . results . fromJust . decode . BL.pack
*Main> let tops = top5shows ans
*Main> head tops
Top5Show AA [2015-07-22,2015-07-08,2015-06-22,...]
*Main> last tops
Top5Show ZSPH [2015-09-10,2015-10-07,2015-10-22,2015-11-05,2015-11-06]
let's codify that:
--}

justRows :: QueryResult -> [TableRow]
justRows = rows . head . results . fromJust . decode . BL.pack
