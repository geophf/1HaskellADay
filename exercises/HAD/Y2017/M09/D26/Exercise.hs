{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D26.Exercise where

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
import Y2017.M09.D22.Exercise (scanArticles, dir, arts, rawText)
import Y2017.M09.D25.Exercise (parseArticle, metadata)

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

insertRawNames :: Query
insertRawNames = [sql|INSERT INTO name_stg (article_id,names) VALUES (?,?)|]

-- from the above, derive the below:

instance ToRow RawNames where
   toRow rn = undefined

-- Okay, great, and you can use the inserter from before to construct the
-- procedure that inserts RawNames values into PostgreSQL database.

-- For all the articles compressed in the archive, (art ++ dir), insert the
-- names from the Person metadata into the names_stg table at the index artId.

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
