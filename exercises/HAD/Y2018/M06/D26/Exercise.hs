{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D26.Exercise where

{--
You have a properties file and you wish to convert it to JSON to make a REST
call with it as POST data.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)

-- the properties file is at

exDir, propsFile :: FilePath
exDir = "Y2018/M06/D26/"
propsFile = "full-etc.properties"

-- read in the JSON file and print it out as pretty JSON

props2JSON :: FilePath -> IO ()
props2JSON file = undefined

-- which means converting these properties into ... something? a Properties?
-- Nah: a Map already converts to a (JSON) Value, so just do that:

lines2Map :: [String] -> Map String String
lines2Map lines = undefined

-- and with a Map you can encodePretty it.
