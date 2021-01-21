{-# LANGUAGE OverloadedStrings #-}

module Y2021.M01.D20.Solution where

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Aeson.WikiDatum

import Graph.Query
import Graph.JSON.Cypher

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy.Char8 as BL

{--
We have a JSON file from wikidata with wineries and their geo-locations.

Parse it.
--}

data Winery = Winery { winery :: WikiDatum, location :: LongLat }
   deriving (Eq, Ord, Show)

instance FromJSON Winery where
   parseJSON = withObject "Winery" $ \v ->
      Winery <$> v *: "item" <*> v @: "location"

type Wineries = Map Name Winery

wineriesJSON, wineriesDir :: FilePath
wineriesJSON = "winery-locations.json"
wineriesDir = "Y2021/M01/D20/"

readWineries :: FilePath -> IO Wineries
readWineries json = 
   Map.fromList . map (name . winery &&& id) . fromMaybe [] . decode
               <$> BL.readFile json

-- How many wineries are there?

{--
>>> readWineries (wineriesDir ++ wineriesJSON)
fromList [("21 Cellars",
           Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q4630984",
                   name = "21 Cellars"},
                   location = point({ latitude: 47.2675, longitude: -122.471 })}),
          ("Aaron Wines",
           Winery {winery = WD {qid = "http://www.wikidata.org/entity/Q63964494", 
                   name = "Aaron Wines"}, 
                   location = point({ latitude: 35.5762243, longitude: -120.690493 })}),...]
>>> let wineries = it
>>> Map.size wineries
610
--}
