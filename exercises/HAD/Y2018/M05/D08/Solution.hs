{-# LANGUAGE ViewPatterns #-}

module Y2018.M05.D08.Solution where

{--
Okay, now that we have the new articles downloaded from the REST endpoint and
the ArticleMetaData context from the database, let's do some triage!

So, just like with the ArticleMetaData, we have to generalize the Article-
TriageInformation type from the specific Package and (Dated)Article types to
types that allow us to switch from Pilot(-specific) articles to WPJ, or, in the
future, articles of any type, ... or so we hope.

Whilst still being useful for solving this problem, too.

So, without further ado:
--}

import Control.Arrow ((&&&))
import Control.Monad (mplus)

import Data.Aeson (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time

import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Control.DList
import Control.Logic.Frege ((-|))

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM

import Store.SQL.Connection
import Store.SQL.Util.AuditLogging (oneWeekAgo)
import Store.SQL.Util.Indexed hiding (idx)

-- import Y2018.M01.D30.Solution -- template for our ArticleTriageInformation

import Y2018.M04.D02.Solution  -- for Article type
import Y2018.M04.D13.Solution  -- for Packet type
import Y2018.M05.D04.Solution  -- for articles from the rest endpoint
import Y2018.M05.D07.Solution  -- for article context from the database

data Triage = NEW | UPDATED | REDUNDANT
   deriving (Eq, Ord, Show)

instance Monoid Triage where
   mempty = REDUNDANT
   mappend = undefined

-- Triage is the dual of a Semigroupoid: we only care about its unit value
-- in logic analyses.

data ArticleTriageInformation package block article idx =
   ATI { pack    :: package,
         article :: (block, article),
         amd     :: Maybe (IxValue (ArticleMetaData idx)) }
      deriving Show

{--
The output from our analysis of the article context from the database (the
ArticleMetadata set) and the articles downloaded from the REST endpoint (the
ParsedPackage values) is the triaged article set
--}

type WPJATI = ArticleTriageInformation (Packet Value) Value Article Integer

triageArticles :: [IxValue (ArticleMetaData Integer)] -> [ParsedPacket]
               -> Map Triage [WPJATI]
triageArticles amd = ta' (Map.fromList (map (artId . val &&& id) amd)) MM.empty

type MapAMD a = Map a (IxValue (ArticleMetaData a))

{--
So, with the above.

1. download the last week's articles from the REST endpoint
2. get the article metadata context from the database
3. triage the downloaded articles.

1. How many new articles are there?
2. How many updated articles are there?
3. How many redundant articles are there?

Since the World Policy Journal doesn't publish many articles each day, and a
packet has 100 articles, there 'may' be a lot of redundant articles. Do your
results bear this out?
--}

ta' :: MapAMD Integer -> MultiMap Triage WPJATI (DList WPJATI) -> [ParsedPacket]
    -> Map Triage [WPJATI]
ta' _ result [] = Map.map dlToList (MM.store result)
ta' context accum ((PP (p, arts)):packs) =
   ta' context (foldr (uncurry MM.insert) accum (map (classify context p) arts))
       packs

-- for article a in packet p:
--     a id date no update     in context -> redundant
--     a id date _         not in context -> new
--     a id date update        in context -> updated or redundant

classify :: MapAMD Integer -> Packet Value -> (Value, Article) -> (Triage, WPJATI)
classify context p (v, arti) =
   let atx = art arti in
   fromJust ((Map.lookup (idx atx) context >>= \amd ->
              return (cmpDates amd atx, ATI p (v, arti) (Just amd)))
        `mplus` (Just (NEW, ATI p (v, arti) Nothing)))

-- actually fromJust-mplus-ground term is just fromMaybe
-- and the lhs of the the fromJust is an fmap over the Map.lookup.

-- It's embarrassing how I complicate simple things.

cmpDates :: IxValue (ArticleMetaData a) -> Article' -> Triage
cmpDates (lastUpdate . val -> amd) (updated -> art) = cd' amd art

cd' :: Maybe Day -> Maybe ZonedTime -> Triage
cd' Nothing (Just _) = UPDATED
cd' (Just amd) Nothing = REDUNDANT
cd' (Just amd) (Just art) = 
   amd < localDay (zonedTimeToLocalTime art) -| UPDATED

{--
>>> conn <- connectInfo WPJ >>= connect
>>> (wk, pp) <- downloader conn
>>> amd <- fetchArticleMetaData conn wk
>>> close conn
>>> mtri = triageArticles amd pp
>>> Map.map length mtri
fromList [(NEW,9),(UPDATED,3),(REDUNDANT,2)]

TA-DAH!

That took some finessing, but there it is!
--}
