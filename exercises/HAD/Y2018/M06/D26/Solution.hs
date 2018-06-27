{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D26.Solution where

{--
You have a properties file and you wish to convert it to JSON to make a REST
call with it as POST data.
--}

import Control.Arrow (second)

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map

-- the properties file is at

exDir, propsFile :: FilePath
exDir = "Y2018/M06/D26/"
propsFile = "full-etc.properties"

-- read in the JSON file and print it out as pretty JSON

props2JSON :: FilePath -> IO ()
props2JSON file = readFile file >>=
    BL.putStrLn . encodePretty . lines2Map . rmDedLinez . lines

-- which means converting these properties into ... something? a Properties?
-- Nah: a Map already converts to a (JSON) Value, so just do that:

lines2Map :: [String] -> Map String String
lines2Map = Map.fromList . map (second (drop 2) . break ( == ':'))

-- and with a Map you can encodePretty it.

rmDedLinez :: [String] -> [String]
rmDedLinez [] = []
rmDedLinez ("":rest) = rmDedLinez rest
rmDedLinez (h:t) = h:rmDedLinez t

{--
>>> props2JSON (exDir ++ propsFile)
{
    "Date": "Apr 12, 2018",
    "Publication": "The Virginian-Pilot",
    "Author": "Brock Vergakis ",
    "ID": "3ef89d52-3e5b-11e8-9c06-6bfe63c66422",
    "Title": "Pair of Norfolk-based ships to go to the Big Apple for Fleet Week New York",
    "Full_Text": "<div itemprop=\"articleBody\" class=\"..."
}
--}
