{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D26.Exercise where

import qualified Codec.Compression.GZip as GZ
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

import Y2017.M09.D20.Solution (inserter)
import Y2017.M09.D22.Exercise (scanArticles, dir, arts, rawText, articleTextById)
import Y2017.M09.D25.Exercise (parseArticle, metadata, Article, artId)

{--
So, I wrote a whole exercise for today, which you will see as tomorrow's 
exercise, instead, because then I realized that it was too much for one day 
without some introduction.

So, today's Haskell problem: a name, by any other name, would smell as sweet.

A datum from Friday's exercise is "Person":

>>> articles <- scanArticles . GZ.decompress <$> BL.readFile (dir ++ arts)
>>> Just art3 = parseArticle 3 (rawText $ head articles)
>>> Map.lookup "People" (metadata art3)
Just "Cuomo, Mario M"

This is an example where the article is about just one person. As you scan the
articles you will see some that are about more than one person and the names
will be in various formats.

Today we will not worry about formats of name(s) in the Person field.

Because today we're simply going to store the names.

Say you have a staging table in PostgreSQL called name_stg with the following
structure:
--}

data RawNames = Raw { fromArticle :: Int, text :: String }
   deriving (Eq, Ord, Show)

-- with our handy insert statement:

insertRawNamesStmt :: Query
insertRawNamesStmt = [sql|INSERT INTO name_stg (article_id,names) VALUES (?,?)|]

-- from the above, derive the below:

instance ToRow RawNames where
   toRow rn = undefined

{-- 
Okay, great, and you can use the inserter from before to construct the
procedure that inserts RawNames values into PostgreSQL database.

Before we do that, we have to convert articles scanned to a list of raw names 
values. And before we do that, let's create a function that pipelines the whole
process of extracting articles from the archive and reifying those articles
to the Y2017.M09.D25.Article type.
--}

type Compressed = ByteString
-- reminder to me that this is a compressed archive

extractArticles :: Compressed -> [Article] 
extractArticles gz = undefined

-- then let's grab the line that has the raw names listed from each article

art2RawNames :: Article -> RawNames
art2RawNames art = undefined

-- and with that transformation function, we can insert raw names from articles

insertAllRawNames :: Connection -> [RawNames] -> IO ()
insertAllRawNames conn = inserter conn insertRawNamesStmt

-- How many rows did you insert? [low key: your answer should be '11']

-- Now: how many names did you insert?

-- We will address that question tomorrow when we get into some simple name
-- parsers.

{-- BONUS -----------------------------------------------------------------

Output your RawNames values as JSON.

--}

instance ToJSON RawNames where
   toJSON rn = undefined

-- BONUS-BONUS ------------------------------------------------------------

-- prettily.
