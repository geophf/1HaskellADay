{-# LANGUAGE OverloadedStrings #-}

module Y2016.M08.D18.Solution where

import Data.Aeson
import Data.Maybe (mapMaybe)

import Graph.JSON.Cypher.Read
import Graph.JSON.Cypher.Read.Graphs

import Y2016.M08.D15.Exercise (twitterGraphUrl)

{--
{"id":"1033","labels":["Link"],"properties":{"url":"http://logicaltypes.blogspot.com/2016/05/april-2016-1haskelladay-problem-and.html"}
--}

data URL = URI { url :: String } deriving (Eq, Ord, Show)

instance FromJSON URL where
   parseJSON (Object o) = URI <$> o .: "url"

readTwitterURLs :: FilePath -> IO [URL]
readTwitterURLs =
   fmap (map URI . mapMaybe ((\(PJ o) -> o <<-$ "url") . propsn)
      . filter ((elem "Link") . labels) . concatMap nodes) . readGraphJSON

{--
*Y2016.M08.D18.Solution> readTwitterURLs twitterGraphUrl ~> urls
[URI {url = "http://logicaltypes.blogspot.com/2016/05/april-..."}, ...]

Question: how many URLs are in this graph-JSON

*Y2016.M08.D18.Solution> length urls ~> 18
--}
