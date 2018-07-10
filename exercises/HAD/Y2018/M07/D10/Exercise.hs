{-# LANGUAGE OverloadedStrings #-}

module Y2018.M07.D10.Exercise where

{--
So, yesterday we explored JSON structure

(EVERYTHING IS A MAP! ... EXCEPT WHAT ISN'T, BUT OKAY!)

Today we're going to explore TWO (much smaller) JSON structures and transform
one to another.

Because, like XML, JSON is all about transformation, baybee!
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map (Map)

-- the input JSON (being output from an analysis tool)

exDir, input :: FilePath
exDir = "Y2018/M07/D10/"
input = "output.json"

-- yeah, the input is the output of the analysis tool. Deal.

{--
The input is in the following format:

Map EntityName { wiki_info:  Wiki, scores: [related articles], queryEnt: Int }

where:
--}

data Wiki = Wikt { wname, wtitle, wurl :: String,
                   wtext, wsum, wimg :: Maybe String }
   deriving Show

instance FromJSON Wiki where
   parseJSON (Object o) = undefined

type EntityName = String

type Input = Map EntityName Analysis

-- and out Analysis is a composition of the wiki info, scores, and query

data Analysis = Ysis { wikt :: Wiki, scores :: [Value], query :: Double }
   deriving Show

{--
Now, you would think this would Just Work(tm). And it would, if this were 
well-structured JSON.

But it's not well-structured JSON. Check out this embedded entry:

    "big brother": {
        "scores": "",
        "query_entity_score": ""
    },

wut. So much for well-structured JSON. How do we deal with this? I don't know.

I think what we have to do is to stage the parsing into ProtoAnalysis then
convert ProtoAnalysis to Analysis iff it has wiki_info. Let's try that.
--}

data ProtoAnalysis = PA { paWik :: Maybe Wiki, paScores, paQuery :: Value }
   deriving Show

instance FromJSON ProtoAnalysis where
   parseJSON (Object o) = undefined

readProto :: FilePath -> IO (Map EntityName ProtoAnalysis)
readProto file = undefined

-- That will work. Now we convert a Proto to Analysis

proto2analysis :: ProtoAnalysis -> Maybe Analysis
proto2analysis prot = undefined

-- then we sequence the result to get our Input value from the JSON

readInputJSON :: FilePath -> IO Input
readInputJSON file = undefined

-- What is your result? How many entries does your Input Map have?
