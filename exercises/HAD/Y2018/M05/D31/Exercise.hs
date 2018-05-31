{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M05.D31.Exercise where

{--
Yesterday we looked at Big(ish) Data. You know what looking at Big(ish) Data
leads to? MORE Big(ish) Data ... WITH analytics. But no charts today. WAH!

So, yesterday we did a little bit of categorization of our Big Data set. Boss
Man comes back and says: "Nice, but I want ..."

Isn't that always the case? You do your job, even add a bit more for complete-
ness sake, and they now they want more, more, more!

I guess that's why they pay us the Big Bucks.

So, today, we'll take our set, broken up into Buckets, and return the sections
of interest: GRANDE and VENTI and do a bit of analysis on those buckets, trans-
forming our input JSON into ... well: JSON. BUT ANAYLZED JSON!

Woot.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty

import Data.Map (Map)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2018.M05.D30.Exercise

-- from the BucketList yesterday, produce a smaller bucket list of only the
-- GRANDE and VENTI elements:

superSizeMe :: BucketList a -> BucketList a
superSizeMe bl = undefined

-- Now let us add our analysis to the JSON. For each Article add a pair
-- "entity_count" so we can see (quickly) how many entities are in the
-- JSON and output that as JSON, prettily

data Article' a = Art' { entityCount :: Int, art :: Article a }
   deriving (Eq, Ord, Show)

art2art' :: Article a -> Article' a
art2art' art = undefined

instance ToJSON a => ToJSON (Article' a) where
   toJSON art' = undefined

printBucketList :: FilePath -> BucketList a -> IO ()
printBucketList output bl = undefined

-- prints the GRANDE and VENTI elements JSON enhanced with entity_count

{-- BONUS -----------------------------------------------------------------

Boss man wants to see the original articles to see if there's some connection
between article content and length and the time it takes to extract the
entities.

Go to the Pilot database, extract the full text of the articles for GRANDE
and VENTI, insert those articles into the JSON. Then declare Miller Time (tm)
--}

fullTextStmt :: Query
fullTextStmt = [sql|SELECT id,full_text FROM article WHERE id in (?)|]

data Str = S String
   deriving (Eq, Show)

instance FromRow Str where
   fromRow = undefined

fullTexts :: Connection -> [Index] -> [IxValue Str]
fullTexts conn idxn = undefined

-- of course, we need to extract the indices from the article set to do above

articleIndex :: Article a -> Index
articleIndex art = undefined

data Article2 a = Art2 { text :: String, meta :: Article' a }
   deriving (Eq, Ord, Show)

-- find some way to match up articles with their texts and merge them

arts2arts2 :: BucketList a -> [IxValue Str] -> Map Integer (Article2 a)
arts2arts2 arts texts = undefined

instance ToJSON a => ToJSON (Article2 a) where
   toJSON art = undefined

-- and write that out as pretty-printed JSON

printBucketListWithText :: FilePath -> BucketList a -> [IxValue Str] -> IO ()
printBucketListWithText output bl texts = undefined
