{-# LANGUAGE OverloadedStrings #-}

module Y2017.M11.D21.Exercise where

{--
Okey-dokey. From yesterday's exercise, we're returning full articles with 
key-phrases and ranked and everything!

Boss-man: I don't need full article text, just a summary of the first 140
characters. And remove the special characters. And drop the key-phrases: we 
don't need that in the front-end.

Okay, then! Let's do that!

From yesterday's results, do the above. Capice?
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Time

-- below imports available 1HaskellADay git repository

import Y2017.M11.D01.Exercise -- for special character filtration
import Y2017.M11.D03.Exercise -- for Strength
import Y2017.M11.D07.Exercise -- for Recommendation
import Y2017.M11.D20.Exercise -- for article sets filtered by keyword search

data Brief =
   Summarized { briefIdx :: Integer, date :: Day, title, summary :: String,
                rank :: Strength }
      deriving (Eq, Show)

rec2brief :: Recommendation -> Brief
rec2brief rec = undefined

-- also: WATCH OUT for special characters in the title

summarize :: SpecialCharTable -> String -> String
summarize special text = undefined

-- summarize truncates the full text to the first 140 characters, because we're
-- HALF of a tweet. ... yup.

{--
JSON structure is:

article_id
article_date
article_title
article_summary
article_rank
article_view_count
--}

instance ToJSON Brief where
   toJSON brief = undefined

{-- BONUS -----------------------------------------------------------------

Write an app that does just like yesterday's app does: returns a set of articles
from a set of keyword filters, but this time returns briefs-as-JSON
--}

main' :: [String] -> IO ()
main' keywords = undefined
