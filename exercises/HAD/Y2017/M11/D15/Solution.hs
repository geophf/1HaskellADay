{-# LANGUAGE OverloadedStrings #-}

module Y2017.M11.D15.Solution where

{--
OOH! MONGODB!

Today's Haskell Exercise we're going to dive into a NoSQL storage alternative,
MongoDB, parsing a CSV input file and uploading that information as JSON to
the mongod.

So.

1. Scan in the CSV file as a structure.
--}

-- import Data.Aeson
-- import Data.Aeson.Encode.Pretty

import Data.Bson
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock
import Numeric (readDec)

-- below imports available via 1HaskellADay git repository

import Control.List (weave)
import Control.Scan.CSV

data Article =
   Art { artIdx :: Integer, title :: String, published :: Day, 
         author, summary :: String, img, url :: FilePath,
         views, rank :: Integer,
         keywords :: [String] }
      deriving (Eq)

-- we sure have a lot of article-types floating around! Good thing I can keep
-- all those disparate types compartmentalized, eh?

-- *sigh*

csv2Art :: Monad m => String -> m Article
csv2Art = c2a . csv

c2a :: Monad m => [String] -> m Article
c2a [idx, tit, pub, auth, sum, img, url, vie, rnk] =
   return (Art (read idx) tit (parseDate pub) auth sum img 
               url (read (filter isDigit vie)) (read rnk)
               (words "education funding"))
c2a line = error ("Couldn't parse " ++ show line)

-- parseDate lifted from Stack Overflow at:
-- https://stackoverflow.com/questions/4174372/haskell-date-parsing-and-formatting

parseDate :: String -> Day
parseDate s = fromGregorian (2000 + y) m d
    where [(m,rest) ] = readDec s
          [(d,rest1)] = readDec (tail rest)
          [(y, _)   ] = readDec (tail rest1)

articleSet :: FilePath
articleSet = "Y2017/M11/D15/edweek.csv"

readArticles :: FilePath -> IO [Article]
readArticles = fmap (concatMap csv2Art . drop 4 . lines) . readFile

{--
Now that we have the articles, convert them into the following JSON-format

{
        "_id" : ObjectId("59f1413cd8ffa2cbebe6add0"),
        "articleId" : "101",
        "ArticleName" : "Donald Trump wants speedy ACA repeal, replacement",
        "author" : "Leslie Small",
        "publicationDate" : ISODate("2017-01-10T00:00:00Z"),
        "summary" : "President-elect Donald Trump says that he wants to both repeal and replace the Affordable Care Act quickly, a position that contradicts what Republican leaders in Congress have planned for the healthcare law.",
        "imgPath" : [
                "https://qtxasset.com/styles/breakpoint_xl_880px_w/s3fs/2016-11/Donald-J-Trump.jpeg?Z4gkRYMNz0f636qTSpcdn_LkxkwXT130&itok=z8u50OzP"
        ],
        "articleUrl" : "http://www.fiercehealthcare.com/payer/donald-trump-wants-speedy-aca-repeal-replacement",
        "numViews" : 1000000,
        "ranking" : 41,
        "keywords" : [
                "trump",
                "affordable care act",
                "reform"
        ]
}

for MongoDB _id is optional: if you don't provide one MongoDB makes one.
--}

art2doc :: Article -> Document
art2doc art = ["articleId" := s (show (artIdx art + 200)),
               "ArticleName" := s (title art),
               "author" := s (author art),
               "publicationDate" := UTC (UTCTime (published art) 0),
               "summary" := s (summary art),
               "imgPath" := Array [s (img art)],
               "articleUrl" := s (url art), 
               "numViews" := Int32 (fromIntegral (views art)),
               "ranking" := Int32 (fromIntegral (rank art + 80)),
               "keywords" := Array (map s (keywords art))]
   where s = String . T.pack

-- welp.

instance Show Article where
   show art =
      unlines ( "{" :comma(map (('\t':) . showField) (art2doc art)) ++ ["}"])

comma :: [String] -> [String]
comma [elt] = [elt]
comma (h:t) = (h ++ ",") : comma t

showField :: Field -> String
showField f = show (label f) ++ " : " ++ showVal (value f)

showVal :: Value -> String
showVal (String s) = show s
showVal (Int32 n) = show n
showVal (UTC d) = "ISODate(\"" ++ showDate d ++ "\")"
showVal (Array a) = brackets "[" "]" (map showVal a)

showDate :: UTCTime -> String
showDate (UTCTime d _) = show d ++ "T00:00:00Z"

brackets :: String -> String -> [String] -> String
brackets a b c = a ++ weave c ++ b

-- and now that we've output our articles to JSON, upload them to the
-- mongod using Haskell-mongod MAGIC! ... or not. AWS security issues
