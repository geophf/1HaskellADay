{-# LANGUAGE OverloadedStrings #-}

module Wikidata.Query.Aeson where

-- We take a wikidata query result as JSON and reify it into Haskell values

-- Scary imports below, but the net result is you get a list of values back
-- from a wikidata query.

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (toList)
import Data.HashMap.Lazy ((!))
import Data.Text (Text)
import Data.Vector (Vector)

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

-- Now, for types that need to drill into the JSON to reify themselves:

parseVal :: FromJSON a => Value -> Text -> Parser a
parseVal = parseDepth "value"

parseDepth :: FromJSON a => Text -> Value -> Text -> Parser a
parseDepth sub (Object o) key = o .: key >>= (.: sub)

{-- e.g.:

data StateCapital = StateCap { state, capital :: String }
  deriving (Eq, Ord, Show)

instance FromJSON StateCapital where
   parseJSON o = StateCap <$> parseVal o "stateLabel"
                          <*> parseVal o "capitalLabel"
--}
