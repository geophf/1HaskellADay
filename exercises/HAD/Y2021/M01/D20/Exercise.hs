{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D20.Exercise where

import Data.XHTML.KML
import Data.Aeson.WikiDatum

import Graph.Query
import Graph.JSON.Cypher

import Data.Map (Map)

{--
We have a JSON file from wikidata with wineries and their geo-locations.

Parse it.
--}

type Winery = String
type Wineries = Map Winery LongLat

wineriesJSON, wineriesDir :: FilePath
wineriesJSON = "winery-locations.json"
wineriesDir = "Y2021/M01/D21/"

readWineries :: FilePath -> IO Wineries
readWineries = undefined

-- How many wineries are there?
