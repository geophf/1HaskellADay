{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D02.Solution where

import Data.Aeson
import Data.Aeson.Types (Parser)

import Data.Aeson.WikiDatum (Name)

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher (matchSet, Cypher)

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

{--
With this neo4j cypher query against a CSV file of wine-tasters and wines:


LOAD CSV WITH HEADERS 
FROM 'https://raw.githubusercontent.com/lju-lazarevic/wine/master/data/winemag-data-130k-v2.csv' AS row
WITH CASE row.taster_twitter_handle
        WHEN null
        THEN null
        ELSE [row.taster_name, row.taster_twitter_handle]
    END as twitterz
    RETURN DISTINCT twitterz

we get the following file:
--}

twitterzDir, twitterzJSON :: FilePath
twitterzDir = "Y2021/M02/D02/"
twitterzJSON = "twitterz.json"

fetchTwitterz :: FilePath -> IO [Taster]
fetchTwitterz file = fromMaybe [] . decode <$> BL.readFile file

data Taster = Taster { name :: Name, twitter :: Name }
   deriving (Eq, Ord, Show)

instance Node Taster where
   asNode t = constr "Taster" [("name", name t)]

toTaster :: [Value] -> Parser Taster
toTaster [a,b] = Taster <$> parseJSON a <*> parseJSON b

{--
data T' = T' Taster
   deriving Show

instance FromJSON T' where
   parseJSON = withObject "twitterz" $ \v -> T' <$> v .: "twitterz"

t12t :: T' -> Taster
t12t (T' t) = t
--}

instance FromJSON Taster where
   -- parseJSON = withArray "twitterz" (toTaster . V.toList)
   parseJSON = withObject "twitterz" $ \v -> (v .: "twitterz" >>= toTaster)

{--
>>> fetchTwitterz (twitterzDir ++ twitterzJSON)
[Taster {name = "Roger Voss", twitter = "@vossroger"},..]
>>> let tasters = it
--}

-- using the matchSet function, upload the twitter handles to the graph store

{--
>>> :t matchSet 
matchSet
  :: (Show b, Node a) =>
     Graph.JSON.Cypher.Var
     -> a -> Attribute b -> Graph.JSON.Cypher.Cypher

>>> graphEndpoint 
...
>>> let url = it
--}

toCyph :: Taster -> Cypher
toCyph t@(Taster _ twit) = matchSet "t" t ("twitter", twit)

{--
>>> getGraphResponse url (map toCyph tasters)
"{\"results\":...,\"errors\":[]}"
--}
