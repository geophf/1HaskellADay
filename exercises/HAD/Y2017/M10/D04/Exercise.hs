{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M10.D04.Exercise where

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

import Data.Map (Map)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Data.Bag
import Store.SQL.Connection (connectInfo)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

-- using the below query, extract the Subject table into an IxValue list

fetchSubjectStmt :: Query
fetchSubjectStmt = [sql|SELECT * from subject|]

type Subject = IxValue String

fetchSubjects :: Connection -> IO [Subject]
fetchSubjects conn = undefined

-- next, fetch the article_subject table into [Pivot]

fetchArtSubjStmt :: Query
fetchArtSubjStmt = [sql|SELECT (article_id,subject_id) from article_subject|]

fetchSubjPivots :: Connection -> IO [Pivot]
fetchSubjPivots conn = undefined

-- With the pivots we can work 'backwards' as it were.

-- What are the top 5 most mentioned subjects, by name? Include the number of
-- times each subject was mentioned (the return type is a hint)

top5SubjectMentions :: [Subject] -> [Pivot] -> Bag String
top5SubjectMentions subjs pvts = undefined

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
   fromRow = undefined

fetchArticleBySubjStmt :: Query
fetchArticleBySubjStmt =
   [sql|SELECT (id,title,publish_dt) FROM article
        WHERE id IN SELECT article_id FROM article_subject
                    WHERE subject_id IN ?|]

fetchArticlesBySubj :: Connection -> [Integer] -> IO [ArticleSummary]
fetchArticlesBySubj conn subjIds = undefined

{--
Now we have date, article (with title), and subject.

Several ways to slice up this horse. So, use whatever graphical representation
to show weight of article subject by date, or popularity of a topic, and the
timespan for that, ... or whatever you'd like. Here's one such representation:
--}

type Title = String
type Topic = String

type Grouping = Map Topic (Map Day Title)

graphTopics :: [Subject] -> [Pivot] -> [ArticleSummary] -> Grouping
graphTopics subjs pivs sums = undefined

-- Once you have this mapping, you can output the result graphically as:

-- 1. 3D scatterplot of topics by date
-- 2. Concentric circles: topics containing dates containing articles
-- 3. N-dimentional graph of Topics x Dates x Articles

-- you choose, or create your own data visualization

visualize :: Grouping -> IO ()
visualize groups = undefined
