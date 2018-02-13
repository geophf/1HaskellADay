{-# LANGUAGE DeriveAnyClass, OverloadedStrings #-}

module Y2018.M02.D12.Solution where

{--
We have CSV stored locally here. Parse it.

Of course, it's more 'fun' than just parsing CSV, so, read on!
--}

import qualified Codec.Compression.GZip as GZ

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time

import System.Environment

-- below imports available via 1HaskellADay git repository

import Control.DList (dlToList, dl')
import Control.List (weave)
import Control.Scan.CSV

import Data.HTML hiding (text)
import qualified Data.MultiMap as MM

-- the file is:

artsDir :: FilePath
artsDir = "Y2018/M02/D12/"

csvFile :: FilePath
csvFile = "25_random_articles.csv"

{--
How many rows are in the data set? How many columns? Does each row have the
same number of columns?

Of course, you didn't read in the first row, because that's column names.

Okay, great! Now let's parse in the information into a Haskell type based off
the column information.
--}

data Article =
   Art { idx :: Integer,
         published :: Day,
         artId, title, url, text :: String,
         keywords :: [String] }
      deriving (Eq, Show)

-- From a row of data, parse in an article:

parseArt :: [String] -> Article
parseArt [i,p,a,_,_,t,u,txt,k] =
   Art (read i) (read p) a t u txt (map removeLeadingSpace $ csv k)
parseArt weird = error ("Could not parse article from " ++ weave weird)

removeLeadingSpace :: String -> String
removeLeadingSpace (' ':rest) = removeLeadingSpace rest
removeLeadingSpace str = str

{-- 
Super! Are we done?

One more thing

We don't want to be looking at HTML, we want the plain text of it, so, do that.
--}

instance HTML Article where body = pure . text

removeTags :: Article -> Article
removeTags art = art { text = unlines (plainText art) }

{-- BONUS -----------------------------------------------------------------

Output these rows as (pretty-printed) JSON
--}

instance ToJSON Article where
   toJSON (Art i p a t u h k) =
      object ["idx" .= i,"published" .= p,"articleId" .= a,"keywords" .= k,
              "url" .= u,"title" .= t,"body" .= h]

instance FromJSON Article where
   parseJSON (Object o) =
      Art <$> o .: "idx" <*> o .: "published" <*> o .: "articleId"
          <*> o .: "title" <*> o .: "url" <*> o .: "body" <*> o .: "keywords"

data Articles = Arts { arts :: [Article] }
   deriving (Eq, Show)

instance ToJSON Articles where
   toJSON (Arts a) = object ["arts" .= a]

instance FromJSON Articles where
   parseJSON (Object o) = Arts <$> o .: "arts"

-- and tie that all together:

csv2json :: FilePath -> FilePath -> IO ()
csv2json csvin jsonout =
   readFile csvin >>=
   BL.writeFile jsonout . encodePretty . Arts 
                        . map (removeTags . parseArt . csv) . tail . lines

json2Arts :: FilePath -> IO Articles
json2Arts file = fromJust . decode . GZ.decompress <$> BL.readFile file

mapify :: [Article] -> Map String Article
mapify = Map.fromList . map (artId &&& id)

{--
>>> csv2json csvFile (artsDir ++ "arts.json")

$ gzip "Y2018/M02/D12/arts.json"

... which we will be looking at in tomorrow's exercise.
--}
