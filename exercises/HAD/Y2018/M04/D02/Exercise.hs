{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D02.Exercise where

import Data.Aeson
import Data.Aeson.Encode.Pretty

{--
Today we're going to be doing a bit of structural discovery in JSON. We have
JSON at:
--}

arts :: FilePath
arts = "Y2018/M04/D02/posts.json"

-- part 1: read in the JSON. (how?)

readJSON :: FilePath -> IO Value
readJSON file = undefined

-- part 2: write out the JSON prettily

writeJSON :: FilePath -> Value -> IO ()
writeJSON outfile json = undefined

-- part 3: tease out some structure. You tell me how this works by declaring
-- a data structure of what looks important to you in the data set and parsing
-- the JSON into that data structure

data Article = YourStructureDeclaration
   deriving (Eq, Show)

instance FromJSON Article where
   parseJSON (Object o) = undefined

-- What is the structure you came up with?

-- Tomorrow we'll look at storing these data in a PostgreSQL database
