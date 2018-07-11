{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M07.D11.Exercise where

{--
So, yesterday we loaded in a JSON structure that was a wee-bit quirky

Today we're going to transform that structure that's a mite less quirky

Because, like XML, JSON is all about transformation, baybee!
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Time

-- the below imports are available via 1HaskellADay git repository

import Y2018.M07.D10.Exercise

-- So, load in the input JSON structure and transform it into the below form:

{--
>>> jasn <- readInputJSON (exDir ++ input)
--}

output :: FilePath
output = "structure-we-want.json"

data Output = SomeOtherStructureWeWantToEndUpWith

instance FromJSON Output where
   parseJSON json = undefined

-- yeah, we have to read in the output to get what we want to output. Deal.

in2out :: Input -> Output
in2out innie = undefined

-- in2out converts the input data in the input JSON structure to the output
-- structure.

-- And with that, we output our output structure as outputly-structured JSON

instance ToJSON Output where
   toJSON out = undefined

-- read in the input data from the input file, read in the output structure
-- from the output file, transform the input to output structure, then
-- write out the result to file:

xform :: FilePath -> FilePath -> FilePath -> IO ()
xform inputData outputStructure output2file = undefined
