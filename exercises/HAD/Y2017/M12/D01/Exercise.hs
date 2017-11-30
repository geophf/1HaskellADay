{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D01.Exercise where

{--
Today's exercise adds a bit of polish on yesterday's solution. Yesterday, we
extracted the source article and recommended articles, all as briefs. Good.

However, the workflow is to show the entire source article, then brief the
recommended articles, so, we need to do that, instead:
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)

import Y2017.M11.D01.Exercise  -- for SpecialCharTable
import Y2017.M11.D21.Exercise  -- for Brief
import Y2017.M11.D22.Exercise  -- for FullText
import Y2017.M11.D29.Exercise  -- for Publish
import Y2017.M11.D30.Exercise  -- for Publish extraction

data Print = ToPrint { srcArticle :: FullText, recArticles :: [Brief] }
   deriving (Eq, Show)

instance ToJSON Print where
   toJSON articleSet = undefined

-- so, from a set of to-be-published article Ids, get the Print copy:

sendToPrinter :: SpecialCharTable -> Connection -> [Publish] -> IO Print
sendToPrinter chars conn recs = undefined

{-- BONUS -----------------------------------------------------------------

Write an app that takes the src article ID, looks up the to-be-printed articles
recommended for it (hint: see Y2017.M11.D30), and outputs Print result as JSON.
--}

main' :: [String] -> IO ()
main' args = undefined
