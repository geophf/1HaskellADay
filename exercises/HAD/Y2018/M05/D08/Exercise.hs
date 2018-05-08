module Y2018.M05.D08.Exercise where

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

import Data.Aeson (Value)
import Data.Map (Map)

import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed

-- import Y2018.M01.D30.Exercise -- template for our ArticleTriageInformation

import Y2018.M04.D02.Exercise  -- for Article type
import Y2018.M04.D13.Exercise  -- for Packet type
import Y2018.M05.D04.Exercise  -- for articles from the rest endpoint
import Y2018.M05.D07.Exercise  -- for article context from the database

data Triage = NEW | UPDATED | REDUNDANT
   deriving (Eq, Ord, Show)

instance Monoid Triage where
   mempty = REDUNDANT
   mappend = undefined

-- Triage is the dual of a Semigroupoid: we only care about its unit value
-- in logic analyses.

data ArticleTriageInformation package block article idx =
   ATI { pack :: package,
         art  :: (block, article),
         amd  :: Maybe (IxValue (ArticleMetaData idx)) }
      deriving Show

{--
The output from our analysis of the article context from the database (the
ArticleMetadata set) and the articles downloaded from the REST endpoint (the
ParsedPackage values) is the triaged article set
--}

type WPJATI = ArticleTriageInformation (Packet Value) Value Article Int
type MapTri = Map Triage WPJATI

triageArticles :: [IxValue (ArticleMetaData Int)] -> [ParsedPacket] -> MapTri
triageArticles amd packets = undefined

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
