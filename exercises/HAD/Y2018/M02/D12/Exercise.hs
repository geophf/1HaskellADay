{-# LANGUAGE OverloadedStrings #-}

module Y2018.M02.D12.Exercise where

{--
We have CSV stored locally here. Parse it.

Of course, it's more 'fun' than just parsing CSV, so, read on!
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map (Map)
import Data.Time

-- below imports available via 1HaskellADay git repository

import Control.List (weave)
import Control.Scan.CSV

import Data.HTML

-- the file is:

artsDir :: FilePath
artsDir = "Y2018/M02/D12/"

csvFile :: FilePath
csvFile = artsDir ++ "25_random_articles.csv"

{--
How many rows are in the data set? How many columns? Does each row have the
same number of columns?

Of course, you didn't read in the first row, because that's column names.

Okay, great! Now let's parse in the information into a Haskell type based off
the column information.
--}

data Article =
   Art { idx                     :: Integer,
         published               :: Day,
         artId, title, url, text :: String,
         keywords                :: [String] }
      deriving (Eq, Show)

-- From a row of data, parse in an article:

parseArt :: [String] -> Article
parseArt [ix,published,artId,_,_,title,url,txt,kws] = undefined
parseArt weird = error ("Could not parse article from " ++ weave weird)

{-- 
Super! Are we done?

One more thing

We don't want to be looking at HTML, we want the plain text of it, so, do that.
--}

instance HTML Article where
   body art = undefined

removeTags :: Article -> Article
removeTags art = undefined

{-- BONUS -----------------------------------------------------------------

Output these rows as (pretty-printed) JSON
--}

instance ToJSON Article where
   toJSON art = undefined

data Articles = Arts { arts :: [Article] }
   deriving (Eq, Show)

instance ToJSON Articles where
   toJSON arts = undefined

-- and tie that all together:

csv2json :: FilePath -> FilePath -> IO ()
csv2json csvin jsonout = undefined

{-- BONUS-BONUS -----------------------------------------------------------

We've written the csv out as JSON via the Article value. Now let's verify that
what we've written out we can read later.
--}

instance FromJSON Article where
   parseJSON (Object o) = undefined

instance FromJSON Articles where
   parseJSON (Object o) = undefined

json2Arts :: FilePath -> IO Articles
json2Arts file = undefined

-- and provide a mapped representation of the articles by article unique id.

mapify :: [Article] -> Map String Article
mapify arts = undefined
