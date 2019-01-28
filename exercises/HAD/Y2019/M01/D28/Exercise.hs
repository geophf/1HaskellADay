{-# LANGUAGE OverloadedStrings #-}

module Y2019.M01.D28.Exercise where

import Data.Aeson
import Data.Time

{--
Happy New Year, Haskellers!

So, we have two files containing articles in different formats:
--}

exDir :: FilePath
exDir = "Y2019/M01/D28/"

articleFiles :: [String]
articleFiles = ["pilot-not-json.json","pilot-ok-txt.json"]

-- The exercise for today: read in these JSON files into Haskell data structures

data Article = Art { artId :: String, published :: Day, text :: String }
   deriving Show

instance FromJSON Article where
   parseJSON art = undefined

readJsonFile :: FilePath -> IO [Article]
readJsonFile file = undefined

-- How many articles are in each JSON file?
