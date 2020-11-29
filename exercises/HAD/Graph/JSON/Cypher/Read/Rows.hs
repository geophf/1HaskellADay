{-# LANGUAGE OverloadedStrings #-}

module Graph.JSON.Cypher.Read.Rows where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)

-- Reads in rows of JSON from a Cypher query result

data GraphResults a = GR { results :: [TabledResults a], errors :: [Errors] }
   deriving Show

instance FromJSON a => FromJSON (GraphResults a) where
   parseJSON (Object v) = GR <$> v .: "results" <*> v .: "errors"

data TabledResults a = Table { columns :: [String], rows :: [TableRow a] }
   deriving Show

instance FromJSON a => FromJSON (TabledResults a) where
   parseJSON (Object v) = Table <$> v .: "columns" <*> v .: "data"

type Errors = Object

data TableRow a = TR { row :: a } deriving Show

instance FromJSON a => FromJSON (TableRow a) where
   parseJSON (Object v) = TR <$> v .: "row"

{--
>>> getGraphResponse url ["match ()-[]->(n) where not (n)-[]->() return n.letter"]
>>> let ans = it
--}

type QueryResult = String

justRows :: FromJSON a => QueryResult -> [TableRow a]
justRows = rows . head . results . fromJust . decode . BL.pack

{--
>>> concat $ map (head . row) ((justRows ans) :: [TableRow [String]])
"HVFLPJOYCXBZQ"
--}

-- to parse pathed result-sets:

unarray :: [Value] -> Parser [Value]
unarray = withArray "an array of arrays" (return . V.toList) . head

{--
Another, more complex, example:

From a sample row returned of:

theLetterQ :: String
theLetterQ = "[[{},{\"rep\":\"-\"},{\"letter\":\"T\"},{\"rep\":\"-\"},"
          ++ "{\"letter\":\"M\"},{\"rep\":\".\"},{\"letter\":\"G\"},"
          ++ "{\"rep\":\"-\"},{\"letter\":\"Q\"}]]"

(note that an array of arrays is returned for paths)

Given the parsers:

instance FromJSON Path where
   parseJSON = withArray "morse code" $ pathify . unarray . V.toList

pathify :: Parser [Value] -> Parser Path
pathify pvs = pvs >>= pathy' [] . tail

pathy' :: [MorsePair] -> [Value] -> Parser Path
pathy' path [] = return (Path $ reverse path)
pathy' acc (a:b:rest) =
   parseJSON a >>= \m ->
   parseJSON b >>= \l ->
   pathy' ((m, l):acc) rest

instance FromJSON Morse where
   parseJSON = withObject "da-dit" $ \v -> v .: "rep" >>= return . read

instance FromJSON Letter where
   parseJSON = withObject "ltr" $ \v -> Chr <$> v .: "letter"

we can do:

>>> getGraphResponse url [cyphQuery]
>>> let padme = (justRows it) :: [TableRow Path]
>>> let nmt = newMorseTable (map row padme)
>>> take 5 $ Map.toList nmt
[('A',.-),('B',-...),('C',-.-.),('D',-..),('E',.)]

>>> Map.size nmt
26

--}
