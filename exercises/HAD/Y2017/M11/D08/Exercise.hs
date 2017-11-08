{-# LANGUAGE OverloadedStrings #-}

module Y2017.M11.D08.Exercise where

{--
Yesterday, you loaded up a set of recommended articles and married them with 
some scoring of those articles ... we left a slot open for the keyphrases used 
in their scoring (which we parsed in Y2017.M11.D03). Today we are going to fill 
that slot.

Given the set of recommended articles:
--}

import Y2017.M11.D07.Exercise

-- their scores:

import Y2017.M11.D06.Exercise

-- and the set of keyphrases associated with (well, all) articles:

import Y2017.M11.D03.Exercise

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

-- first, define a JSON instance for the keyphrases:

instance ToJSON Keyphrase where
   toJSON kw = undefined

-- And now, save out the recommendation with score and keyphrases as JSON

saveRecs :: KeyphraseMap -> FilePath -> [Recommendation] -> IO ()
saveRecs outputfile keyphrases recs = undefined

-- assume recs starts with an empty list for article_keyphrase

-- Use the recommendation file, the score file and the keyphrases file in their
-- respective directories
