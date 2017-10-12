{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, TupleSections #-}

module Y2017.M10.D04.Solution where

{--
Okay, so we've uploaded our data to a data store! Great! Yay!

So.

What're we gonna do with those data? Freeze it in Glacier?

Yes, that's what companies do, but let's try something different: let's actually
use those data and do something!

Today, we'll do something.

Either from the data store or from the compressed archive of articles, get
the subjects for the articles. We've done that, but this time compose a mapping
of subject to their source articles and answer some questions.
--}

import Control.Arrow ((&&&))
import Control.Monad (forM)
import Data.Function (on)
import Data.List (sortOn, groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Store.SQL.Connection (connectInfo)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

-- If you want to visualize your data sets as packed circles:

import Store.SQL.Connection (connectInfo)

-- using the below query, extract the Subject table into an IxValue list

fetchSubjectStmt :: Query
fetchSubjectStmt = [sql|SELECT * from subject|]

type Subject = IxValue String

fetchSubjects :: Connection -> IO [Subject]
fetchSubjects = flip query_ fetchSubjectStmt

{--
>>> connectInfo
ConnectInfo { ... }
>>> conn <- connect it
>>> subjs <- fetchSubjects conn
>>> length subjs
1193
--}

-- next, fetch the article_subject table into [Pivot]

fetchArtSubjStmt :: Query
fetchArtSubjStmt = [sql|SELECT article_id,subject_id from article_subject|]

-- of course we need a FromRow instance for Pivot that we will roll right 
-- into the Store.SQL.Util.Pivot module after this exercise ...

instance FromRow Pivot where
   fromRow = Pvt <$> field <*> field

fetchSubjPivots :: Connection -> IO [Pivot]
fetchSubjPivots = flip query_ fetchArtSubjStmt

{--
>>> pivs <- fetchSubjPivots conn
>>> length pivs
2281

Woo, doggies! I've stored a lot of pivots!
--}

-- With the pivots we can work 'backwards' as it were.

-- What are the top 5 most mentioned subjects, by name? Include the number of
-- times each subject was mentioned (the return type is a hint)

top5SubjectMentions :: [Subject] -> [Pivot] -> Bag String
top5SubjectMentions (Map.fromList . map (idx &&& val) -> subjs) =
   Bag.fromList . mapMaybe (flip Map.lookup subjs . trgId)

{--
>>> let mentions = top5SubjectMentions subjs pivs
>>> length mentions 
976
>>> take 5 (Bag.rank mentions)
[("Social networks",53),("Presidents",40),("Hurricanes",28),("Books",25),("Aliens",23)]
--}

{-- BONUS -----------------------------------------------------------------

Okay, we dig deeper into the data and plot subject by date mentioned. Whoa!

We have the ability to parse articles from compressed archives on disk or from
an URL, but now we need to retrieve our articles from the database.

We may eventually dig into the full text, but right now we're getting shape or
structure of the data. We have subjects associated with articles, but what
dates do they occur? Where are the concentrations? The article has a date
published, so let's get that.
--}

data ArticleSummary =
   ArtSum { artId :: Integer, title :: String, published :: Day }
      deriving (Eq, Show)

instance FromRow ArticleSummary where
   fromRow = ArtSum <$> field <*> field <*> field

instance Indexed ArticleSummary where
   idx = artId

fetchArticleBySubjStmt :: Query
fetchArticleBySubjStmt =
   [sql|SELECT id,title,publish_dt FROM article
        WHERE id IN SELECT article_id FROM article_subject
                    WHERE subject_id IN ?|]

fetchArticlesBySubj :: Connection -> [Integer] -> IO [ArticleSummary]
fetchArticlesBySubj conn = query conn fetchArticleBySubjStmt

{--
Now we have date, article (with title), and subject.

Several ways to slice up this horse. So, use whatever graphical representation
to show weight of article subject by date, or popularity of a topic, and the
timespan for that, ... or whatever you'd like. Here's one such representation:
--}

type Title = String
type Topic = String

type Grouping = Map Topic (Map Day [Title])

graphTopics :: [Subject] -> [Pivot] -> [ArticleSummary] -> Grouping
graphTopics subjs pivs sums =

-- welp.

   let arts = Map.fromList (map (artId &&& id) sums) in
   Map.fromList (map (\(IxV ixSubj subj) ->

{--
my first stab:

   Map.fromList (forM subjs (\(IxV ixSubj subj) ->
      let articles = filter ((== ixSubj) . trgId) pivs
          filts    = mapMaybe (flip Map.lookup arts . srcId) articles
          dates    = articlesByDate filts
      in  return (subj, Map.fromList dates)))

yeah, I crashed and burned, type-wise, so broke out and typed each part below:
--}

      let sums = articles arts (artIdBySubj ixSubj pivs) in
      (subj, Map.fromList (articlesByDate sums))) subjs)

articles :: Map Integer ArticleSummary -> [Integer] -> [ArticleSummary]
articles arts = mapMaybe (flip Map.lookup arts)

artIdBySubj :: Integer -> [Pivot] -> [Integer]
artIdBySubj subjId = map srcIx . filter ((== subjId) . trgId)

articlesByDate :: [ArticleSummary] -> [(Day, [Title])]
articlesByDate arts =

-- we need to convert [[(d1,t1), (d1,t2)], [(d2,t3)], ...] to
--                    [(d1, [t1, t2]), (d2, [t3]), ...]

   map (fst . head &&& map snd)
       (groupBy ((==) `on` fst) (sortOn fst (map (published &&& title) arts)))

{--
>>> let graph = graphTopics subjs pivs arts
>>> length graph
976

Sweet! I didn't have time to do a visual on this. We'll do this tomorrow.
--}
