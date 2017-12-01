{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D01.Solution where

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

import Control.Scan.Config
import Store.SQL.Connection (withConnection)

import Y2017.M11.D01.Solution  -- for SpecialCharTable
import Y2017.M11.D21.Solution  -- for Brief
import Y2017.M11.D22.Solution  -- for FullText
import Y2017.M11.D29.Solution  -- for Publish
import Y2017.M11.D30.Solution  -- for Publish extraction

data Print = ToPrint { srcArticle :: FullText, recArticles :: [Brief] }
   deriving (Eq, Show)

instance ToJSON Print where
   toJSON (ToPrint src recs) =
      object ["source_article" .= src, "recommendations" .= recs]

-- so, from a set of to-be-published article Ids, get the Print copy:

sendToPrinter :: SpecialCharTable -> Connection -> [Publish] -> IO Print
sendToPrinter chars conn recs@(Pub src _ _:_) =
   ToPrint <$> (cleanup chars . head <$> fullText conn src)
           <*> briefs chars conn recs

{-- BONUS -----------------------------------------------------------------

Write an app that takes the src article ID, looks up the to-be-printed articles
recommended for it (hint: see Y2017.M11.D30), and outputs Print result as JSON.
--}

main' :: [String] -> IO ()
main' [srcId] =
   do homeDir <- home
      chars <- readSpecialChars (homeDir ++ "/.specialChars.prop")
      withConnection (\conn -> 
         fetchPublish conn (read srcId) >>= sendToPrinter chars conn >>=
         BL.putStrLn . encodePretty)
main' _ = putStrLn (unlines ["", "printer <artId>", "", "\tGives the article "
             ++ "and the recommended articles to be published as JSON", ""])
