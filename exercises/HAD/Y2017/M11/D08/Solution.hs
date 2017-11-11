{-# LANGUAGE OverloadedStrings #-}

module Y2017.M11.D08.Solution where

{--
Yesterday, you loaded up a set of recommended articles and married them with 
some scoring of those articles ... we left a slot open for the keywords used in 
their scoring (which we parsed in Y2017.M11.D03). Today we are going to fill 
that slot.

Given the set of recommended articles:
--}

import Y2017.M11.D07.Solution

-- their scores:

import Y2017.M11.D06.Solution

-- and the set of keyphrases associated with (well, all) articles:

import Y2017.M11.D03.Solution

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map

-- first, define a JSON instance for the keyphrases:

{--
instance ToJSON Keyphrase where
   toJSON (KW strength (SQS str)) =
      object ["strength" .= strength, "keyphrase" .= str]

-- move this definition to Y2017.M11.D07.Solution
--}

-- And now, save out the recommendation with score and keyphrases as JSON

saveRecs :: FilePath -> KeyphraseMap -> [Recommendation] -> IO ()
saveRecs outputfile keyphrases =
   BL.writeFile outputfile . showRecs keyphrases

showRecs :: KeyphraseMap -> [Recommendation] -> ByteString
showRecs keyphrases = encodePretty
   . map (\rec ->
               rec { scoreKWs = keyphrases Map.! fromIntegral (scoreIdx rec) })

-- assume recs starts with an empty list for article_keyphrase

-- Use the recommendation file, the score file and the keyphrases file in their
-- respective directories

{--
>>> scores <- readScoreFile scoreFile
>>> recs <- readRecs "Y2017/M11/D07/recommend.json"
>>> marriage = marry recs scores
>>> kws <- readCompressedKeyphrases (kwDir ++ "kw_index_file.txt.gz")
>>> length kws
9835

... 9835 articles indexed with keyphrases

>>> saveRecs "Y2017/M11/D08/recommends.json" kws marriage

... and BOOM! we have our recommendations with keyphrases and scores, printed 
prettily!
--}
