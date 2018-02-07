{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D30.Solution where

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

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad.Writer (runWriterT)

import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe,fromMaybe)
import Data.Monoid
import Data.Time
import Data.Time.Calendar 
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Control.DList
import Control.List (singleton)
import Control.Logic.Frege ((-|))

import qualified Data.MultiMap as MM

import Store.SQL.Connection (connectInfo, withConnection)

import Y2017.M12.D20.Solution -- for Packet
import Y2017.M12.D27.Solution (DatedArticle)
import qualified Y2017.M12.D27.Solution as Art
import Y2018.M01.D04.Solution (Authors)
-- import Y2018.M01.D26.Solution (pa)  -- hint for parsing articles
import Y2018.M01.D26.Solution (ow)
import Y2018.M01.D29.Solution (oneWeekAgo)

data Triage = NEW | UPDATED | REDUNDANT
   deriving (Eq, Ord, Show)

instance Monoid Triage where
   mempty = REDUNDANT
   mappend = undefined

-- the metadata on the articles that we need to download includes the past
-- week's articles, but only some metadata:

data ArticleMetaData =
   AMD { artId :: Integer, uuid :: String,
         published, lastUpdated :: Maybe Day }
      deriving (Eq, Show)

instance FromRow ArticleMetaData where
   fromRow = AMD <$> field <*> field <*> field <*> field

fetchArticleMetaDataStmt :: Query
fetchArticleMetaDataStmt =
   [sql|SELECT id,article_id,publish_dt,update_dt FROM article
        WHERE  publish_dt > ?|]

fetchArticleMetaData :: Connection -> Day -> IO [ArticleMetaData]
fetchArticleMetaData conn = query conn fetchArticleMetaDataStmt . singleton

-- (low-key: if it were I, I'd add an extra couple of days to ensure we didn't
--  mistake older downloaded articles as redundant)

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> owa <- oneWeekAgo conn
>>> term = addDays (-1) owa
>>> term
2018-01-05
>>> packs <- ow owa 0 []
>>> length packs
11

So, now we're all caught up.

>>> amd <- fetchArticleMetaData conn term
>>> length amd
243
--}

-- SO!

-- 1. from the packet-set downloaded from the REST endpoint, get the articles

articles :: [Packet] -> [(Packet, (Block, DatedArticle Authors))]
articles = concatMap (sequence . (id &&& mapMaybe parseArticle . rows))

{--
>>> arts = articles packs
>>> length arts
1096
--}

parseArticle :: Block -> Maybe (Block, DatedArticle Authors)
parseArticle = sequence . (id &&& pars . fromJSON)

pars :: Result (DatedArticle Authors) -> Maybe (DatedArticle Authors)
pars (Error _) = Nothing
pars (Success art) = Just art

-- 2. triage the articles

data ArticleTriageInformation =
   ATI { pack :: Packet, 
          art :: (Block, DatedArticle Authors),
          amd :: Maybe ArticleMetaData }
      deriving Show

triageArticles :: [ArticleMetaData] -> [(Packet, (Block, DatedArticle Authors))]
               -> Map Triage [ArticleTriageInformation]
triageArticles metadata =
    Map.map dlToList . MM.store . MM.fromList dl'
                     . map (triageWith (mapify metadata))

-- why the information-intensive type? Well, down the road, when I'm updating
-- UPDATED articles in the database, I need to know what the article id is.
-- And, of course, NEW articles do not have article metadata

{--
>>> bins = triageArticles amd arts
>>> Map.map length bins
fromList [(NEW,885),(UPDATED,23),(REDUNDANT,188)]
--}

triageWith :: Map String ArticleMetaData
           -> (Packet, (Block, DatedArticle Authors))
           -> (Triage, ArticleTriageInformation)
triageWith metadata thing@(p, b@(bl,a)) =
   fromMaybe (NEW, ATI p b Nothing)
             (tria thing <$> Map.lookup (Art.uuid a) metadata)

tria :: (Packet, (Block, DatedArticle Authors)) -> ArticleMetaData
     -> (Triage, ArticleTriageInformation)
tria (p, b@(_, a)) amd@(AMD _ _ pub up) =
   (cmpDates (if Nothing == up then (Art.starttime a, pub)
              else (Art.lastupdated a, up)), ATI p b (Just amd))

cmpDates :: (Maybe ZonedTime, Maybe Day) -> Triage
cmpDates (Nothing, _)                     = REDUNDANT
cmpDates (Just zt, Nothing)               = UPDATED
cmpDates (Just (ZonedTime zt _), Just dt) = localDay zt > dt -| UPDATED

mapify :: [ArticleMetaData] -> Map String ArticleMetaData
mapify = Map.fromList . map (uuid &&& id)

-- 3. create a packet that covers the articles in NEW and UPDATED

megapacket :: Map Triage [ArticleTriageInformation] -> Packet
megapacket metadata =
   let (p:acks) = map pack (concat (Map.elems (Map.delete REDUNDANT metadata)))
       pck      = foldr reducePackets p acks
   in  pck { count = length acks + 1 }

{--
>>> pckd = megapacket bins
>>> pckd
Pack {view = "default", count = 908, total = 288768, next = 1100, prev = Nothing, rows = []}
--}

reducePackets :: Packet -> Packet -> Packet
reducePackets (Pack _ _ t' n' p' _) (Pack v _ t n p _) =
   Pack v 0 (max t' t) (max n' n) (liftA2 min p p') []

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
main' []     = errmsg
main' ["go"] = withConnection (\conn -> do
   owa <- oneWeekAgo conn
   putStrLn ("Fetching articles back to " ++ show owa ++ " from REST endpoint")
   let term = addDays (-1) owa
   (packs, log) <- runWriterT (ow owa 0 [])
   putStrLn ("Fetched " ++ show (length packs) ++ " packets")
   amd <- fetchArticleMetaData conn term
   print (Map.map length . triageArticles amd $ articles packs))
main' _      = errmsg

errmsg :: IO ()
errmsg = putStrLn (unlines ["", "check_dailies <go>", "",
   "\tprints status of articles from REST endpoint",
   "\t\tstatus: NEW, UPDATED, REDUNDANT"])
