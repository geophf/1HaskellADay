{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D22.Solution where

{--
You send me an article id, I send you the full text of the article.

Simple as that.

... of course, it's never as simple as that, is it.

The article needs to be cleaned of special characters. Also, some articles
have this annoying "Source: http://" whatever at the bottom of their text that
needs to be removed.

So, you know: do that.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isPrefixOf)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Control.Scan.Config
import Store.SQL.Connection

import Y2017.M11.D01.Solution -- for special chars

data FullText = FT { ftidx :: Integer,
                     fttitle, ftfullText :: String,
                     ftauthor :: Maybe String,
                     ftdate :: Day }
   deriving (Eq, Show)

instance FromRow FullText where
   fromRow = FT <$> field <*> field <*> field <*> field <*> field

-- we also want to output our results as JSON

instance ToJSON FullText where
   toJSON ft = object ["article_id" .= ftidx ft,
                       "article_title" .= fttitle ft,
                       "article_date" .= ftdate ft,
                       "article_author" .= ftauthor ft,
                       "article_body" .= ftfullText ft]

-- from an id get the article's full text

fullTextStmt :: Query
fullTextStmt =
   [sql|SELECT id,title,full_text,author,publish_dt FROM article where id=?|]

-- returns a cleaned full text of article x

fullText :: Connection -> Integer -> IO [FullText]
fullText conn idx = query conn fullTextStmt [idx]

-- now, we need to remove special characters and remove the 'Source:'-thingie

removeSourceThingie :: SpecialCharTable -> String -> String
removeSourceThingie dict = refineString dict . elSplito "Source URL http"

elSplito :: String -> String -> String
elSplito cutter [] = []
elSplito cutter str@(h:t) =
   if cutter `isPrefixOf` str then [] else h:elSplito cutter t

-- so now we need to transform a FullText value to a cleaned-up verson of
-- the same

cleanup :: SpecialCharTable -> FullText -> FullText
cleanup dict (FT a b c d e) =
   FT a (refineString dict b) (removeSourceThingie dict c) d e

-- And we are g2g! AM I RIGHT OR AM I RIGHT?

main' :: [String] -> IO ()
main' [id] = do
   homeDir <- home
   special <- readSpecialChars (homeDir ++ "/.specialChars.prop")
   withConnection (\conn -> fullText conn (read id) >>=
                   BL.putStrLn . encodePretty . cleanup special . head)

main' anythingElse = putStrLn $ unlines ["", "full_bodied <articleId>", "",
   "\tfetches the text for <articleId>"]
