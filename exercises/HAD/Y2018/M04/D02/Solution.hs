{-# LANGUAGE OverloadedStrings #-}

module Y2018.M04.D02.Solution where

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
import Data.Time

-- the below imports are available via 1HaskellADay git repository

import Data.HTML

{--
Today we're going to be doing a bit of structural discovery in JSON. We have
JSON at:
--}

artsDir, arts :: FilePath
artsDir = "Y2018/M04/D02/"
arts = artsDir ++ "posts.json"

-- part 1: read in the JSON. (how?)

readJSON :: FilePath -> IO Value
readJSON = fmap (fromJust . decode) . BL.readFile

{--
>>> json <- readJSON arts
--}

-- part 2: write out the JSON prettily

writeJSON :: FilePath -> Value -> IO ()
writeJSON outfile = BL.writeFile outfile .  encodePretty

{--
>>> writeJSON "Y2018/M04/D02/pretty.json" json
--}

-- part 3: tease out some structure. You tell me how this works by declaring
-- a data structure of what looks important to you in the data set and parsing
-- the JSON into that data structure

data Article' =
   Art' { idx                     :: Int,
          date                    :: Maybe ZonedTime,
          title, excerpt, content :: Value,
          tags, categories        :: [Int],
          link                    :: FilePath }
      deriving Show

instance FromJSON Article' where
   parseJSON obj@(Object o) =
      Art' <$> o .: "id"    <*> parseDate obj
           <*> o .: "title" <*> o .: "excerpt"    <*> o .: "content"
           <*> o .: "tags"  <*> o .: "categories" <*> o .: "link"

{--
>>> (Success ans) = (fromJSON json) :: Result [Article']
>>> idx (head ans)
22378
>>> date (head ans)
Nothing
--}

fromRendered :: (Article' -> Value) -> Article' -> Parser (String, String)
fromRendered f =
   fmap (id &&& mbdemark)
      . (\obj -> obj >>= (\(Object o) -> o .: "rendered")) . pure . f

-- nicked from Y2017.M12.D27.Solution:

-- so, but how do we get from that wild and wonderful structure in the JSON
-- for dates to a Haskell Day value?

iso8601like :: String
iso8601like = "%FT%T"

parseDate :: Value -> Parser (Maybe ZonedTime)
parseDate (Object o) =
   o.:? "date_gmt" >>= \mbstr -> return (case mbstr of
         Nothing -> Nothing
         Just t  -> parseTimeM True defaultTimeLocale iso8601like t)

showDate :: ZonedTime -> String
showDate = formatTime defaultTimeLocale iso8601like

{--
sampleDate :: ByteString
sampleDate = BL.unlines ["{",
                "\"rfc2822\": \"Tue, 12 Dec 2017 22:00:00 -0500\",",
                "\"utc\": \"1513134000000\",",
                "\"iso8601\": \"2017-12-12T22:00:00-05:00\"",
            "}"]
--}

-- note that changing -05:00 to -04:00 does NOT change the time zone

mbdemark :: String -> String
mbdemark str@('<':_) = demark str
mbdemark str         = str

rend :: (Article' -> Value) -> Article' -> Parser String
rend f = fmap snd . fromRendered f

data Article = Art { art :: Article', plain, html, titl, summ :: String }
   deriving Show

art2art :: Article' -> Parser Article
art2art art =
   fromRendered content art >>= \(h,p) ->
   Art art p h <$> rend title art <*> rend excerpt art

-- so to get from a Value to an Article, we go by way of an Article'

instance FromJSON Article where
   parseJSON obj@(Object o) = parseJSON obj >>= art2art

{--
>>> (Success ans) = (fromJSON json) :: Result [Article]
>>> titl (head ans)
"Displaced in Darfur"
>>> take 100 (plain (head ans))
"By Ahmed H. Adam \n Thirteen years ago, in March 2005, the U.N. Security Council  proclaimed  that th"
>>> take 100 (html (head ans))
"<p><strong>By Ahmed H. Adam</strong></p>\n<p>Thirteen years ago, in March 2005, the U.N. Security Cou"

YAY!
--}

-- What is the structure you came up with?

-- Tomorrow we'll look at storing these data in a PostgreSQL database
-- Well, actually for tomorrow. Do you see the weirdness with authors? They
-- are all identified as 29, even though the body of the articles have different
-- authors identified. We'll look at this problem tomorrow.
