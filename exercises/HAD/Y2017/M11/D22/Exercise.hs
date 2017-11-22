{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D22.Exercise where

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
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Control.Scan.Config
import Store.SQL.Connection

import Y2017.M11.D01.Exercise -- for special chars

data FullText = FT { ftidx :: Integer,
                     fttitle, ftfullText :: String,
                     ftauthor :: Maybe String,
                     ftdate :: Day }
   deriving (Eq, Show)

instance FromRow FullText where
   fromRow = undefined

-- we also want to output our results as JSON

instance ToJSON FullText where
   toJSON ft = undefined

-- from an id get the article's full text

fullTextStmt :: Query
fullTextStmt = [sql|SELECT id,author,full_text FROM article where id=?|]

-- returns a cleaned full text of article x

fullText :: Connection -> Integer -> IO [FullText]
fullText conn idx = undefined

-- now, we need to remove special characters and remove the 'Source:'-thingie

removeSourceThingie :: SpecialCharTable -> String -> String
removeSourceThingie fulltext = undefined

-- With that, we need to transform a FullText value to a cleaned-up verson of
-- the same

cleanup :: SpecialCharTable -> FullText -> FullText
cleanup dict (FT a b c d e) = undefined

-- And we are g2g! AM I RIGHT OR AM I RIGHT?

main' :: [String] -> IO ()
main' args = undefined
