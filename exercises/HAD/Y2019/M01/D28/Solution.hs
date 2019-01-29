{-# LANGUAGE OverloadedStrings #-}

module Y2019.M01.D28.Solution where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map,elems)
import Data.Maybe (fromJust)
import Data.Time

{--
Happy New Year, Haskellers!

So, we have two files containing articles in different formats:
--}

exDir :: FilePath
exDir = "Y2019/M01/D28/"

articleFiles :: [String]
articleFiles = ["pilot-not-json.json","pilot-ok-txt.json"]

-- The exercise for today: read in these JSON files into Haskell data structures

data Article = Art { artId :: String, published :: Day, text :: String }
   deriving Show

instance FromJSON Article where
   parseJSON (Object o) = Art <$> o .: "article_id" <*> o .: "publish_dt"
                              <*> o .: "rendered_text"

json2Map :: ByteString -> Maybe (Map String [Article])
json2Map = decode

readJsonFile :: FilePath -> IO [Article]
readJsonFile = fmap (concat . elems . fromJust . json2Map) . BL.readFile

-- How many articles are in each JSON file?

{--
>>> a <- readJsonFile (exDir ++ articleFiles !! 0)
>>> length a
10
>>> take 2 $ map artId a
["21a4c856-8051-11e8-bf46-27dc511934cc","0748ff56-82de-11e8-8958-dbe00b71b67d"]

>>> b <- readJsonFile (exDir ++ articleFiles !! 1)
>>> length b
10
> take 2 $ map artId b
["0d1d1530-86c5-11e8-9fde-5f9dc5fbf337","e2b6907e-8628-11e8-afac-2fb3b705ca23"]
--}
