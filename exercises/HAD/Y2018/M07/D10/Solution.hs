{-# LANGUAGE OverloadedStrings #-}

module Y2018.M07.D10.Solution where

{--
So, yesterday we explored JSON structure

(EVERYTHING IS A MAP! ... EXCEPT WHAT ISN'T, BUT OKAY!)

Today we're going to explore TWO (much smaller) JSON structures and transform
one to another.

Because, like XML, JSON is all about transformation, baybee!
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Scientific (toRealFloat)
import Data.Time

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
   parseJSON (Object o) =
      Wikt <$> o .: "Entity" <*> o .: "Page_title" <*> o .: "WikiURL"
           <*> o .: "Full_text" <*> o .: "WikiSummary" <*> o .: "WikiImg"

type EntityName = String

type Input = Map EntityName Analysis

-- and out Analysis is a composition of the wiki info, scores, and query

data Analysis = Ysis { wikt :: Wiki, scores :: [Value], query :: Double }
   deriving Show

{--
instance FromJSON Analysis where
   parseJSON (Object o) =
      Ysis <$> o .: "wiki_info" <*> o .: "scores" <*> o .: "query_entity_score"

-- So now we can parse our input JSON as a map

readInputJSON :: FilePath -> IO Input
readInputJSON = fmap (fromJust . decode) . BL.readFile

Now, here's a thing:

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
   parseJSON (Object o) =
      PA <$> o .:? "wiki_info" <*> o .: "scores" <*> o .: "query_entity_score"

readProto :: FilePath -> IO (Map EntityName ProtoAnalysis)
readProto = fmap (fromJust . decode) . BL.readFile

-- That worked. Now we convert a Proto to Analysis

proto2analysis :: ProtoAnalysis -> Maybe Analysis
proto2analysis (PA Nothing _ _) = Nothing
proto2analysis (PA (Just wikt) (Array arr) (Number n)) =
   Just (Ysis wikt (toList arr) (toRealFloat n))

-- then we sequence the result

readInputJSON :: FilePath -> IO Input
readInputJSON = fmap (Map.mapMaybe proto2analysis) . readProto

{--
>>> jasn <- readInputJSON (exDir ++ input)
>>> take 4 (Map.keys jasn)
["arlington","arlington national cemetery","chuck prichard","danny russel"]
>>> length jasn 
26
--}
