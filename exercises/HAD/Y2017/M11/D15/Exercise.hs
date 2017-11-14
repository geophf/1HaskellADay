module Y2017.M11.D15.Exercise where

{--
OOH! MONGODB!

Today's Haskell Exercise we're going to dive into a NoSQL storage alternative,
MongoDB, parsing a CSV input file and uploading that information to the mongod.

first of all you have to install the MongoDB modules into your Haskell 
environment:

$ cabal update
$ cabal install mongoDB

So.

1. Scan in the CSV file as a structure.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Database.MongoDB
import System.Environment

-- below imports available via 1HaskellADay git repository

import Control.Scan.CSV

data Article = RowOfCSV

-- we sure have a lot of article-types floating around! Good thing I can keep
-- all those disparate types compartmentalized, eh?

-- *sigh*

csv2Art :: String -> Article
csv2Art line = undefined

articleSet :: FilePath
articleSet = "Y2017/M11/D15/edweek.csv"

readArticles :: FilePath -> IO [Article]
readArticles file = undefined

{--
Now that we have the articles, upload them to the mongoDB with the following
schema as a guide. The database is "articledb" and the collection is "articles"

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

for MongoDB _id is optional: if you don't provide one, MongoDB makes one.

the keywords for this data set is: ["education", "funding"]

recall that art is:
artIdx :: Integer, title :: String, published :: Day,
         author, summary :: String, img, url :: FilePath,
         views, rank :: Integer,
         keywords :: [String] }
      deriving (Eq,Show)

--} 

-- render the articles in the above JSON-format ... which Data.Aeson can't
-- handle because of the ISODate guard.

jsonifyArt :: Article -> String
jsonifyArt art = undefined
