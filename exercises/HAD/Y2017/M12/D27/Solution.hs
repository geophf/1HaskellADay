{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D27.Solution where

{--
Yesterday, we stored the unparsed article set into the database. We also were 
able to parse the HTML content of articles stored in JSON the day prior.

Today, we continue parsing article information and then storing those parsed
data into our database.

From the article, we need to store in the database some stuff. These stuff are:

uuid
title
url
starttime
lastupdated
keywords
sections
authors
prologue
content

Now, DON'T PANIC! (tm)

We'll take parsing these fields one day at a time. The simple parsing fields
we'll do today (the transliteration from a simple field in JSON to a simple
field in Haskell), the more complex fields we'll handle a day at a time in turn.
--}

import Control.Monad (zipWithM)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust, catMaybes)
import Text.HTML.TagSoup

-- below import available via 1HaskellADay git repository

import Control.List (weave)
import Control.Logic.Frege ((<<-))
import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed

import Y2017.M12.D20.Solution (readSample, rows)
import Y2017.M12.D22.Solution hiding (content, title, uuid, readSample, rows)
import Y2017.M12.D26.Solution (insertStagedArt)

{--
You see from the above imported exercise that we have already scanned and parsed
the uuid, the title, and the content of each article. We have three more tricky
parsing exercises: starttime and lastupdated for time, authors, which is already
parsed out a bit for us, and sections.

Let's flesh out our article structure from 'yesterday,' leaving the authors
and sections unparsed for now, but figure out how to ingest the times, then,
with the parsed information, save those articles to our PostgreSQL data store.
--}

import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

data DatedArticle a =
   Carbon { uuid, title, url       :: String,
            prologue               :: Maybe String,
            authors                :: [a],
            starttime, lastupdated :: Maybe ZonedTime,
            sections               :: [String],
            keywords               :: [Value],
            content                :: [String],
            byline                 :: Maybe String }
      deriving Show

-- so, but how do we get from that wild and wonderful structure in the JSON
-- for dates to a Haskell Day value?

iso8601like :: String
iso8601like = "%FT%T%z"

parseDate :: Value -> Parser (Maybe ZonedTime)
parseDate (Object o) =
   o.:? "iso8601" >>= \mbstr -> return (case mbstr of
         Nothing -> Nothing
         Just t  -> parseTimeM True defaultTimeLocale iso8601like t)

showDate :: ZonedTime -> String
showDate = formatTime defaultTimeLocale iso8601like

sampleDate :: ByteString
sampleDate = BL.unlines ["{",
                "\"rfc2822\": \"Tue, 12 Dec 2017 22:00:00 -0500\",",
                "\"utc\": \"1513134000000\",",
                "\"iso8601\": \"2017-12-12T22:00:00-05:00\"",
            "}"]

-- note that changing -05:00 to -04:00 does NOT change the time zone

readArticles :: FilePath -> IO (Packet (DatedArticle Value))
readArticles json = fromJust . decode <$> BL.readFile json

{--
>>> arts <- readArticles "Y2017/M12/D27/one-article.json" 
>>> showDate (starttime . head $ rows arts)
"2017-12-12T22:00:00-0500"
--}

instance FromJSON a => FromJSON (DatedArticle a) where
   parseJSON (Object o) =
      Carbon <$> o .: "uuid" <*> o .: "title" <*> o .: "url"
             <*> o .: "prologue" <*> o .: "authors"
             <*> (o .: "starttime" >>= parseDate)
             <*> (o .: "lastupdated" >>= parseDate)
             <*> o .: "sections" <*> o .: "keywords"
             <*> o .: "content" <*> o .: "byline"

-- we also need to make DatedArticle an HTML instance

instance HTML (DatedArticle a) where
   body = content

-- Now, with that parsed structure, save the Article set to the database

instance ToField a => ToRow (DatedArticle a) where
   toRow art@(Carbon uu ti ur pr au st la se ke co _) =
      [toField la, toField st, toField uu, toField ur, toField (demark <$> pr),
       toField (htmlBlock art), toField (unlines $ plainText art),
       toField (weave se), toField ti] 

-- The insert statement gives the Article structure
-- (also image attached from the Entity-relation diagram)

insertArticleStmt :: Query
insertArticleStmt =
   [sql|INSERT INTO article (src_id,update_dt,publish_dt,article_id,url,
                             abstract,full_text,rendered_text,sections,title,
                             authors)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id|]

ixArt2ixArt :: Index -> (DatedArticle a) -> IxValue (DatedArticle a)
ixArt2ixArt (Idx x) art = IxV x art

insertArts :: ToField a => Connection -> [Index] -> [DatedArticle a] -> IO [Index]
insertArts conn = returning conn insertArticleStmt <<- zipWith ixArt2ixArt

-- from the source article ids in the article_stg table and the parsed articles,
-- store the articles in the database

{-- BONUS -----------------------------------------------------------------

Write an ETL process that reads in the JSON, stores the raw article information
in article_stg (hint: Y2017.M12.D26.Exercise), then stores the parsed article
information with the source article id in the database as well.
--}

parseArticle :: Int -> Value -> IO (Maybe (DatedArticle Value))
parseArticle idx = pa idx . fromJSON

pa :: Int -> Result (DatedArticle Value) -> IO (Maybe (DatedArticle Value))
pa idx (Success art) = 
   putStrLn ("Parsed " ++ uuid art) >> return (Just art)
pa idx (Error err) =
   putStrLn ("Could not parse article " ++ show idx ++ ", error: " ++ err) >>
   return Nothing

main' :: [String] -> IO ()
main' [jsonFilePath] =
   readSample jsonFilePath >>= \pac ->
   let blocks = rows pac in
       zipWithM parseArticle [1..] blocks >>= \arts ->
   withConnection (\conn -> do
      ixs <- insertStagedArt conn blocks
      insertArts conn ixs (catMaybes arts)) >>
   putStrLn ("Wrote " ++ (show $ length blocks) ++ " articles to the database.")
main' _ =
   putStrLn (unlines ["","pilot-etl <json-file-name>", "",
                      "\tLoads json-file-name into the Pilot database", ""])
