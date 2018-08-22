{-# LANGUAGE OverloadedStrings #-}

module Y2018.M08.D22.Exercise where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL

import System.Directory

-- Today's Haskell problem. We have a set of JSON files at

exDir :: FilePath
exDir = "Y2018/M08/D22/files/"

-- what are the names of these JSON files?

jsonfilenames :: FilePath -> IO [FilePath]
jsonfilenames dir = undefined

{--
For each of the files in jsonfilenames, read in the JSON, then write out
the JSON as:

{
    "article_id": (the file name (without the extension)),
    "entities": (the contents of the JSON file)
}

--}

restructureJSON :: FilePath -> IO ()
restructureJSON jsonfile = undefined

{-- BONUS -----------------------------------------------------------------

Set up a REST endpoint, it can be as simple as an echo chamber, send the
restructured JSON to that REST endpoint for processing. For me, I have a REST
endpoint that saves these article analyses to a database. You do what you like.
--}

type Endpoint = FilePath

processJSON :: Endpoint -> FilePath -> IO ()
processJSON url jsonfile = undefined

-- hint: this make look a lot like a POST request from curl.

-- are there any results returned?
