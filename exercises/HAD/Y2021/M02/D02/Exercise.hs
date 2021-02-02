{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D02.Exercise where

import Data.Aeson

import Data.Aeson.WikiDatum (Name)

import Data.Relation

import Graph.Query
import Graph.JSON.Cypher (matchSet)

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
fetchTwitterz = undefined

data Taster = Taster { name :: Name, twitter :: Name }
   deriving (Eq, Ord, Show)

instance Node Taster where
   asNode = undefined

instance FromJSON Taster where
   parseJSON = undefined

-- using the matchSet function, upload the twitter handles to the graph store
