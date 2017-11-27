{-# LANGUAGE OverloadedStrings #-}

module Y2017.M11.D21.Solution where

{--
Okey-dokey. From yesterday's exercise, we're returning full articles with 
key-phrases and ranked and everything!

Boss-man: I don't need full article text, just a summary of the first 140
characters. And remove the special characters. And drop the key-phrases: we 
don't need that in the front-end.

Okay, then! Let's do that!

From yesterday's results, do the above. Capice?
--}

import Control.Monad ((>=>))
import Data.Aeson hiding (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time

-- below imports available 1HaskellADay git repository

import Control.Logic.Frege ((<<-))
import Control.Scan.Config
import Store.SQL.Connection (withConnection)

import Y2017.M11.D01.Solution -- for special character filtration
import Y2017.M11.D03.Solution                -- for Strength
import Y2017.M11.D06.Solution hiding (title) -- for Value
import Y2017.M11.D07.Solution hiding (title) -- for Recommendation
import Y2017.M11.D20.Solution -- for article sets filtered by keyword search

data Brief =
   Summarized { briefIdx :: Integer, date :: Maybe Day,
                title :: String,
                summary :: Maybe String,
                viewCount :: Maybe Integer,
                rank :: Value Strength }
      deriving (Eq, Show)

rec2brief :: SpecialCharTable -> Recommendation -> Brief
rec2brief chrs (Scored idx tit txt dt _ _ vc score) =
   Summarized idx (pure dt) (refineString chrs tit)
              (summarize chrs <$> txt) vc (VAL score)

summarize :: SpecialCharTable -> String -> String
summarize = refineString -- having the rec read a summary now

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
   toJSON brief =
     object ["article_id" .= briefIdx brief, "article_date" .= date brief,
             "article_title" .= title brief, "article_summary" .= summary brief,
             "article_rank" .= val2float (rank brief),
             "article_view_count" .= viewCount brief]

{--
Good idea, but the types returned from (Brief -> a) are different.

 (zipWith (.=) (map ("article_" ++) (words "id date title summary rank"))
--}

{-- BONUS -----------------------------------------------------------------

Write an app that does just like yesterday's app does: returns a set of articles
from a set of keyword filters, but this time returns briefs-as-JSON
--}

main' :: [String] -> IO ()
main' keywords = do
   homeDir <- home
   special <- readSpecialChars (homeDir ++ "/.specialChars.prop")
   withConnection (flip recs keywords >=>
      BL.putStrLn . encodePretty . map (rec2brief special))
