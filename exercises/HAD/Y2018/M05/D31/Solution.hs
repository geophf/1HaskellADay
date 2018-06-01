{-# LANGUAGE OverloadedStrings, QuasiQuotes, TupleSections, ViewPatterns #-}

module Y2018.M05.D31.Solution where

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

import Control.Arrow ((&&&))

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay

import Store.SQL.Connection
import Store.SQL.Util.Indexed hiding (idx)

import Y2018.M05.D30.Solution

-- from the BucketList yesterday, produce a smaller bucket list of only the
-- GRANDE and VENTI elements:

superSizeMe :: BucketList a -> BucketList a
superSizeMe = Map.delete SMALL . Map.delete TALL

-- Now let us add our analysis to the JSON. For each Article add a pair
-- "entity_count" so we can see (quickly) how many entities are in the
-- JSON and output that as JSON, prettily

data Article' a = Art' { entityCount :: Int, art :: Article a }
   deriving (Eq, Ord, Show)

art2art' :: Article a -> Article' a
art2art' = flip Art' <*> length . entities

-- of course we need a ToJSON instance for Article a ...

instance ToJSON a => ToJSON (Article a) where
   toJSON (Art i u e t) =
      object ["index" .= i, "uuid" .= u, "entities" .= e, "time" .= t]

instance ToJSON a => ToJSON (Article' a) where
   toJSON (Art' n art) =
      toJSON (Map.insert "entity_count" (Number (fromIntegral n))
                         (val2Map (toJSON art)))

val2Map :: Value -> Map String Value
val2Map = successful . fromJSON

successful :: Result a -> a
successful (Success a) = a

instance ToJSON Bucket where
   toJSON = String . T.pack . show

instance ToJSONKey Bucket

printBucketList :: FilePath -> BucketList Value -> IO ()
printBucketList output =
   BL.writeFile output . encodePretty . Map.map (map art2art') . superSizeMe

{--
>>> arts <- readArts (exDir ++ entitiesJSON)
>>> printBucketList "Y2018/M05/D31/counts.json" (bucketList arts)

counts.json in this exercise's directory.
--}

{-- BONUS -----------------------------------------------------------------

Boss man wants to see the original articles to see if there's some connection
between article content and length and the time it takes to extract the
entities.

Go to the Pilot database, extract the full text of the articles for GRANDE
and VENTI, insert those articles into the JSON. Then declare Miller Time (tm)
--}

fullTextStmt :: Query
fullTextStmt = [sql|SELECT id,full_text FROM article WHERE id in ?|]

data Str = S String
   deriving (Eq, Show)

instance FromRow Str where
   fromRow = S <$> field

fullTexts :: Connection -> [Index] -> IO [IxValue Str]
fullTexts conn = query conn fullTextStmt . Only . In

-- of course, we need to extract the indices from the article set to do above

articleIndex :: Article a -> Index
articleIndex = Idx . idx

data Article2 a = Art2 { text :: String, meta :: Article' a }
   deriving (Eq, Ord, Show)

-- find some way to match up articles with their texts and merge them

type MI = Map Integer
type MappedArts a = MI (Article2 a)

arts2arts2 :: [Article a] -> [IxValue Str] -> MappedArts a
arts2arts2 (Map.fromList . map (idx &&& id) -> arts) =
   foldr (joinArt arts) Map.empty

joinArt :: MI (Article a) -> IxValue Str -> MappedArts a -> MappedArts a
joinArt arts (IxV k (S str)) = Map.insert k (Art2 str (art2art' (arts Map.! k)))

instance ToJSON a => ToJSON (Article2 a) where
  toJSON (Art2 txt art') =
      toJSON (Map.insert "text" (String (T.pack txt)) (val2Map (toJSON art')))

-- and write that out as pretty-printed JSON

printBucketListWithText :: FilePath -> BucketList Value -> IO ()
printBucketListWithText output (Map.toList . superSizeMe -> bl) =

-- we get the full texts for each row of the bucket list and output the
-- enhanced article set as JSON. EASY!

   withConnection PILOT (\conn ->
      mapM (uncurry (rowIt conn)) bl >>=
      BL.writeFile output . encodePretty . Map.fromList)

rowIt :: Connection -> Bucket -> [Article a] -> IO (Bucket, [Article2 a])
rowIt conn bucket arts =
   fullTexts conn (map articleIndex arts) >>=
   return . (bucket,) . Map.elems . arts2arts2 arts

{--
I thought about doing the below, but ... nah.

data Row a = R (Integer, a)

instance ToJSON a => ToJSON (Row a) where
   toJSON (R (i, v)) = object ["index" .= i, "article" .= v]
--}

{--
>>> printBucketListWithText "Y2018/M05/D31/study.json" (bucketList arts)

Ta-dah! Result included on the filepath above.

P.S.: Boss Man was pleased with the results, so: yay! *throws confetti
--}
