{-# LANGUAGE OverloadedStrings #-}

module Y2017.M11.D07.Exercise where

{--
Yesterday's exercise we read in a set of scored documents; the exercise before,
we parsed a set of keywords associated with articles.

Today continues the parsing exercises. This time we're parsing JSON, so it 
'should' be easy. Right?

Given the structure at recommend.json, parse that structure
--}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)

-- below import available via 1HaskellADay git repository

import Y2017.M11.D06.Exercise

recommendFile :: FilePath
recommendFile = "Y2017/M11/D07/recommend.json"

data Recommend = DeclareThisStructureFromWhatYouSeeInTheJSON

-- Yeah. I went there.

instance FromJSON Recommend where
   parseJSON = undefined

readRecs :: FilePath -> IO (Map Integer Recommend)
readRecs file = undefined

-- how many recommendations are there? How many title have the word 'Trump' in 
-- them?

-- hint: you may want to declare a structure that takes a list of 
-- recommendations.

{-- BONUS -----------------------------------------------------------------

From yesterday's exercise you loaded in a set of scores. Today you have a set
of recommendations ... without scores.

Marry the two. Now, output JSON in the following format:

article_id:       <<- make this an integer, please. smh
article_title:
article_body:
article_date:
article_keywords: <<- a list, leave empty for now
article_score:    <<- score goes here
article_author:   <<- if present
--}

data Recommendation = SomeJSONFormat

marry :: Map Integer Recommend -> Map Integer Score -> [Recommendation]
marry recommendations scores = undefined

-- how many recommendations did you get from that marriage?

-- now, save out the recommendations as JSON:

instance ToJSON Recommendation where
   toJSON = undefined

writeRecs :: FilePath -> [Recommendation] -> IO ()
writeRecs outputFile = undefined

{--
>>> writeRecs "Y2017/M11/D07/recs_with_scores.json" marriage

... and you see the pprinted json of the recommendations with their scores.

Tomorrow, we will add the keywords for these specific articles and output this 
as JSON
--}
