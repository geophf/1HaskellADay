{-# LANGUAGE OverloadedStrings #-}

module Graph.JSON.Cypher.Read.Rows where

import Data.Aeson
import Data.Aeson.Types (Parser)

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust, mapMaybe)

import Data.Set (Set)

import qualified Data.Vector as V

import Control.Map (snarf)

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

From the Cypher query:

>>> getGraphResponse url ["match p=(:START_HERE)-[*]->(l:Letter { letter: 'Q' }) return p"]

is a sample row returned of:

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

-- Here's another decoding approach:

mapBy :: Ord a => ([Value] -> Maybe (a,b)) -> QueryResult -> Map a b
mapBy pairf = Map.fromList . mapMaybe (pairf . row) . justRows

mapIt :: (FromJSON a, FromJSON b, Ord a) => QueryResult -> Map a b
mapIt = mapBy toPair

toPair :: (FromJSON a, FromJSON b) => [Value] -> Maybe (a,b)
toPair [a,b] = fromJSON1 a >>= \alef ->
               fromJSON1 b >>= \beth ->
               return (alef, beth)

fromJSON1 :: FromJSON a => Value -> Maybe a
fromJSON1 = reifySuccess . fromJSON

reifySuccess :: Result a -> Maybe a
reifySuccess (Success a) = Just a
reifySuccess _           = Nothing

-- and, if we want to map rows to a multimap:

multimap :: (FromJSON a, FromJSON b, Ord a, Ord b) => QueryResult -> Map a (Set b)
multimap = multimapBy toPair

multimapBy :: (Ord a, Ord b) =>
              ([Value] -> Maybe (a, b)) -> QueryResult -> Map a (Set b)
multimapBy f = snarf id . map (f . row) . justRows
