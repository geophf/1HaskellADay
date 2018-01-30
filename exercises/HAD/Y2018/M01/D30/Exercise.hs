{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D30.Exercise where

{--
Friday we extracted a week's worth of articles from a REST endpoint. Monday,
we determined from the database how far back we have to go and then revise
how many packets we need to fetch.

Today, we're going to use the packets we downloaded from the REST endpoint AND
we're going to fetch metadata on (some of) the articles we stored in our
database to determine in the downloaded packets which articles are NEW, which
articles are UPDATED, and which articles are REDUNDANT.

There are two cases here:

NEW - that's easy: the uuid/article_id is not in the database

UPDATED / REDUNDANT - Not so easy, but let's break it down.

   the uuid/article_id IS in the database
   the updated_dt ISN'T in the database:
       the publish_dt is the same for both packet and db article: REDUNDANT
       -- idk why the publish_dt would be different, but then:
          published_dt is older in the db: UPDATED otherwise: REDUNDANT
   the updated_dt IS in the database and is older than packet's: UPDATED
   the updated_dt IS in the database and is the same or newer in db: REDUNDANT

Today's Haskell problem is to triage the downloaded packets' articles.

Tomorrow's Haskell problem will be to upload and update the triaged articles.
--}

import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Time
import Data.Time.Calendar 
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Y2017.M12.D20.Exercise -- for Packet
import Y2017.M12.D27.Exercise -- for DatedArticle
import Y2018.M01.D04.Exercise -- for Authors
-- import Y2018.M01.D26.Exercise (pa)  -- hint for parsing articles
import Y2018.M01.D29.Exercise (oneWeekAgo)

data Triage = NEW | UPDATED | REDUNDANT
   deriving (Eq, Ord, Show)

-- the metadata on the articles that we need to download includes the past
-- week's articles, but only some metadata:

data ArticleMetaData =
   AMD { artId :: Integer, uuid :: String, published :: Day,
         lastUpdated :: Maybe Day }
      deriving (Eq, Show)

instance FromRow ArticleMetaData where
   fromRow = undefined

fetchArticleMetaDataStmt :: Query
fetchArticleMetaDataStmt =
   [sql|SELECT id,article_id,publish_dt,update_dt FROM article
        WHERE  publish_dt > ?|]

fetchArticleMetaData :: Connection -> Day -> IO [ArticleMetaData]
fetchArticleMetaData conn weekAgo = undefined

-- (low-key: if it were I, I'd add an extra couple of days to ensure we didn't
--  mistake older downloaded articles as redundant)

-- SO!

-- 1. from the packet-set downloaded from the REST endpoint, get the articles

articles :: [Packet] -> [(Packet, (Block, DatedArticle Authors))]
articles packets = undefined

-- 2. triage the articles

data ArticleTriageInformation =
   ATI { pack :: Packet,
          art :: (Block, DatedArticle Authors),
          amd :: Maybe ArticleMetaData }
      deriving Show

triageArticles :: [ArticleMetaData] -> [(Packet, (Block, DatedArticle Authors))]
               -> Map Triage [ArticleTriageInformation]
triageArticles metadata parsedArticles = undefined

-- why the information-intensive type? Well, down the road, when I'm updating
-- UPDATED articles in the database, I need to know what the article id is.
-- And, of course, NEW articles do not have article metadata

-- before we want to scan our metadata for article triage state, we probably
-- want to map the metadata by uuid:

mapify :: [ArticleMetaData] -> Map String ArticleMetaData
mapify = undefined

-- 3. create a packet that covers the articles in NEW and UPDATED

megapacket :: Map Triage [ArticleTriageInformation] -> Packet
megapacket mappo = undefined

{--
Hint for megapacket. The idea here to to create a packet that fully covers the
articles/blocks found in NEW and UPDATED meaningfully.

So, rows can certainly be populated with the NEW articles but we also cannot
lose sight of the UPDATED articles. Also, what are meaningful values for count,
total, next, and prev?

Or, taking all of the above into account, does it make sense for the rows of
the packet to be an empty list (as we have already parsed the articles, so why
repeat that step?), and save the information of the megapacket then store the
NEW and UPDATED articles separately?

We'll look at these questions further tomorrow.
--}

{-- BONUS -----------------------------------------------------------------

Put it all together into an app that spits out a status of the numbers of
new, updated, and redundant articles from a REST endpoint pull.
--}

main' :: [String] -> IO ()
main' args = undefined
